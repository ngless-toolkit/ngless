//! In-process parallelism for NGLess's own compute (preprocess, count, select, ...).
//!
//! Two things live here:
//!
//!  * A **global thread configuration** set once from the command line (`--jobs`/`--threads`
//!    and `--strict-threads`), mirroring how Haskell drives everything off GHC's
//!    `numCapabilities` (set once via `setNumCapabilities`). The default is a single thread,
//!    matching the Haskell default (`NThreads 1`) and keeping output byte-for-byte
//!    reproducible unless the user opts in.
//!
//!  * [`par_map_ordered`], an **order-preserving** bounded parallel map — the Rust analogue of
//!    conduit's `asyncMapEitherC`. Results come out in input order regardless of which worker
//!    finishes first, so every consumer that feeds it an ordered stream keeps producing
//!    byte-identical output at any thread count.

use std::sync::OnceLock;

struct ThreadConfig {
    /// Worker count for NGLess's own parallel work (`>= 1`).
    n_threads: usize,
    /// `--strict-threads`: reserve one core for NGLess/system work when handing a thread count
    /// to external tools (mirrors `nConfStrictThreads`).
    strict: bool,
}

static CONFIG: OnceLock<ThreadConfig> = OnceLock::new();

/// Initialise the global thread configuration from the parsed command line. Called once at
/// startup; later calls are ignored (mirrors the single global `nglConfiguration`).
pub fn init(n_threads: usize, strict: bool) {
    let _ = CONFIG.set(ThreadConfig {
        n_threads: n_threads.max(1),
        strict,
    });
}

fn config() -> &'static ThreadConfig {
    // Default to a single thread (the Haskell default) so callers reached before `init` — e.g.
    // unit tests — behave deterministically.
    CONFIG.get_or_init(|| ThreadConfig {
        n_threads: 1,
        strict: false,
    })
}

/// Number of worker threads to use for NGLess's own in-process parallel work.
pub fn n_threads() -> usize {
    config().n_threads
}

/// Thread count to pass to an external mapper (`bwa`/`minimap2`). With `--strict-threads` and
/// more than one core, reserve one core for NGLess/system work (mirrors `bwathreads` in
/// `Bwa.hs`); otherwise hand the tool the full count.
pub fn mapper_threads() -> usize {
    let cfg = config();
    if cfg.strict && cfg.n_threads > 1 {
        cfg.n_threads - 1
    } else {
        cfg.n_threads
    }
}

/// Apply `f` to each item of `items`, returning the results **in input order** while running up
/// to `n_threads` invocations concurrently.
///
/// The work proceeds in waves of `n_threads` items: a wave is gathered from `items`, mapped in
/// parallel (via [`std::thread::scope`], so `f` may borrow its environment), and its results are
/// yielded in order before the next wave is gathered. This bounds in-flight memory to one wave
/// and guarantees the output order equals the input order regardless of completion order. With
/// `n_threads == 1` it degrades to a plain serial `map`, so the default path is unchanged.
///
/// Best suited to uniform-cost work units (e.g. fixed-size record blocks): a single slow item
/// stalls its wave, which is an acceptable trade for the simplicity of the barrier model.
///
/// To propagate errors, use `R = NgResult<_>` and let the caller short-circuit on the first
/// `Err` — because results are ordered, that is deterministically the lowest-indexed failure.
pub fn par_map_ordered<T, R, F, I>(items: I, n_threads: usize, f: F) -> impl Iterator<Item = R>
where
    I: Iterator<Item = T> + Send,
    F: Fn(T) -> R + Sync,
    T: Send,
    R: Send,
{
    let n = n_threads.max(1);
    let mut source = items;
    let mut buffer: std::collections::VecDeque<R> = std::collections::VecDeque::new();
    std::iter::from_fn(move || {
        if let Some(r) = buffer.pop_front() {
            return Some(r);
        }
        // Gather up to `n` items for the next wave.
        let mut wave: Vec<T> = Vec::with_capacity(n);
        for _ in 0..n {
            match source.next() {
                Some(t) => wave.push(t),
                None => break,
            }
        }
        if wave.is_empty() {
            return None;
        }
        if n == 1 || wave.len() == 1 {
            // Serial fast path: identical behaviour (and ordering) to a plain `map`.
            for t in wave {
                buffer.push_back(f(t));
            }
        } else {
            let f = &f;
            let wave_results: Vec<R> = std::thread::scope(|s| {
                let handles: Vec<_> = wave.into_iter().map(|t| s.spawn(move || f(t))).collect();
                handles
                    .into_iter()
                    .map(|h| h.join().expect("par_map_ordered worker panicked"))
                    .collect()
            });
            buffer.extend(wave_results);
        }
        buffer.pop_front()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ordered_matches_serial_map() {
        let input: Vec<u64> = (0..1000).collect();
        let serial: Vec<u64> = input.iter().map(|&x| x * x).collect();
        for n in [1usize, 2, 3, 4, 8, 16] {
            let got: Vec<u64> =
                par_map_ordered(input.clone().into_iter(), n, |x| x * x).collect();
            assert_eq!(got, serial, "mismatch at n_threads={n}");
        }
    }

    #[test]
    fn handles_empty_input() {
        let got: Vec<u64> =
            par_map_ordered(Vec::<u64>::new().into_iter(), 4, |x| x).collect();
        assert!(got.is_empty());
    }

    #[test]
    fn wave_smaller_than_thread_count() {
        // Fewer items than threads must still come out in order.
        let got: Vec<i32> = par_map_ordered(vec![10, 20, 30].into_iter(), 8, |x| x + 1).collect();
        assert_eq!(got, vec![11, 21, 31]);
    }

    #[test]
    fn first_error_wins_by_index() {
        // Items 3 and 7 fail; iterating in order, the first Err encountered must be index 3.
        let results = par_map_ordered(0..10, 4, |x: i32| {
            if x == 3 || x == 7 {
                Err(x)
            } else {
                Ok(x)
            }
        });
        let mut first_err = None;
        for r in results {
            if let Err(e) = r {
                first_err = Some(e);
                break;
            }
        }
        assert_eq!(first_err, Some(3));
    }

    #[test]
    fn collect_result_short_circuits() {
        let ok: Result<Vec<i32>, i32> =
            par_map_ordered(0..100, 4, |x: i32| Ok(x)).collect();
        assert_eq!(ok.map(|v| v.len()), Ok(100));
    }
}
