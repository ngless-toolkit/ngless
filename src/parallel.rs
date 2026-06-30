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

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

/// Worker count for NGLess's own parallel work (`>= 1`). Defaults to a single thread (the Haskell
/// default) so callers reached before [`init`] — e.g. unit tests — behave deterministically.
static N_THREADS: AtomicUsize = AtomicUsize::new(1);
/// `--strict-threads`: reserve one core for NGLess/system work when handing a thread count to
/// external tools (mirrors `nConfStrictThreads`).
static STRICT: AtomicBool = AtomicBool::new(false);

/// Initialise the global thread configuration from the parsed command line. Called once at
/// startup (mirrors building the single global `nglConfiguration`).
pub fn init(n_threads: usize, strict: bool) {
    N_THREADS.store(n_threads.max(1), Ordering::Relaxed);
    STRICT.store(strict, Ordering::Relaxed);
}

/// Number of worker threads to use for NGLess's own in-process parallel work.
pub fn n_threads() -> usize {
    N_THREADS.load(Ordering::Relaxed)
}

/// Override the worker thread count after [`init`]. Used by the `batch` standard module, which on
/// load supersedes the command-line `--jobs` value with the batch scheduler's CPU allotment
/// (mirrors the `setNumCapabilities` call in `StandardModules/Batch.hs`). `strict` is left as set
/// by [`init`]. Like `init`, this must run before any parallel work begins.
pub fn override_n_threads(n_threads: usize) {
    N_THREADS.store(n_threads.max(1), Ordering::Relaxed);
}

/// Thread count to pass to an external tool (`bwa`/`minimap2`/`samtools`). With `--strict-threads`
/// and more than one core, reserve one core for NGLess/system work (mirrors `bwathreads` in
/// `Bwa.hs` and `samtoolsthreads` in `Utils/Samtools.hs`); otherwise hand the tool the full count.
pub fn external_tool_threads() -> usize {
    let n = n_threads();
    if STRICT.load(Ordering::Relaxed) && n > 1 {
        n - 1
    } else {
        n
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
    I: Iterator<Item = T>,
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

/// Group a fallible stream into fixed-size blocks, the natural work unit for [`par_map_ordered`]
/// (a single read is too small to amortise hand-off cost; a block of a few thousand is not).
///
/// Each yielded item is `Ok(Vec<_>)` of up to `size` elements, or the first `Err` encountered —
/// at which point iteration is meant to stop (the caller short-circuits, and on error the run is
/// abandoned, so the partially gathered block is irrelevant).
pub fn chunked<I, X, E>(mut it: I, size: usize) -> impl Iterator<Item = Result<Vec<X>, E>>
where
    I: Iterator<Item = Result<X, E>>,
{
    let size = size.max(1);
    let mut done = false;
    std::iter::from_fn(move || {
        if done {
            return None;
        }
        let mut buf = Vec::with_capacity(size);
        for _ in 0..size {
            match it.next() {
                Some(Ok(x)) => buf.push(x),
                Some(Err(e)) => {
                    // Fuse: once an error is seen, stop — the run is being abandoned and any
                    // records gathered before it in this block are irrelevant.
                    done = true;
                    return Some(Err(e));
                }
                None => {
                    done = true;
                    break;
                }
            }
        }
        if buf.is_empty() {
            None
        } else {
            Some(Ok(buf))
        }
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
            let got: Vec<u64> = par_map_ordered(input.clone().into_iter(), n, |x| x * x).collect();
            assert_eq!(got, serial, "mismatch at n_threads={n}");
        }
    }

    #[test]
    fn handles_empty_input() {
        let got: Vec<u64> = par_map_ordered(Vec::<u64>::new().into_iter(), 4, |x| x).collect();
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
        let results = par_map_ordered(
            0..10,
            4,
            |x: i32| {
                if x == 3 || x == 7 {
                    Err(x)
                } else {
                    Ok(x)
                }
            },
        );
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
    fn chunked_groups_and_stops_on_error() {
        let ok: Vec<Result<i32, ()>> = (0..10).map(Ok).collect();
        let blocks: Vec<Result<Vec<i32>, ()>> = chunked(ok.into_iter(), 4).collect();
        assert_eq!(
            blocks,
            vec![Ok(vec![0, 1, 2, 3]), Ok(vec![4, 5, 6, 7]), Ok(vec![8, 9])]
        );
        // An error terminates the stream at the block where it occurs.
        let withbad: Vec<Result<i32, &str>> =
            vec![Ok(0), Ok(1), Err("boom"), Ok(3)].into_iter().collect();
        let blocks: Vec<Result<Vec<i32>, &str>> = chunked(withbad.into_iter(), 4).collect();
        assert_eq!(blocks, vec![Err("boom")]);
    }

    #[test]
    fn collect_result_short_circuits() {
        let ok: Result<Vec<i32>, i32> = par_map_ordered(0..100, 4, |x: i32| Ok(x)).collect();
        assert_eq!(ok.map(|v| v.len()), Ok(100));
    }
}
