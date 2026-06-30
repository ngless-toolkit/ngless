//! The `batch` standard module (mirrors `StandardModules/Batch.hs` and `Utils/Batch.hs`).
//!
//! Importing `batch` is a pure-metadata module — it contributes no functions — but loading it has
//! two load-time effects, both driven by batch-scheduler environment variables:
//!
//!  * it exposes two constants describing the array-job index (`JOBINDEX_OR_0`/`JOBINDEX_VALID`),
//!    read from `LSB_JOBINDEX` (LSF) or `SGE_TASK_ID` (SGE); and
//!  * if the environment advertises a CPU allotment (`OMP_NUM_THREADS`/`NSLOTS`/`LSB_DJOB_NUMPROC`/
//!    `SLURM_CPUS_PER_TASK`), it overrides the worker thread count — mirroring the
//!    `setNumCapabilities` call in `loadModule`, which supersedes the command-line `--jobs` value.
//!
//! The constants are wired through the usual standard-module path (`modules::module_constants` for
//! the types, `interpret::module_constant_values` for the runtime values); the thread-count
//! override is applied in `cli.rs` when the module is encountered in the import list.

use std::env;

/// Number of CPUs requested by the batch scheduler. Returns the value of the first of these
/// environment variables that is set to a parseable integer, in order (mirrors `getNcpus` in
/// `Utils/Batch.hs`, which folds `firstJustM` over the same list).
pub fn get_ncpus() -> Option<usize> {
    [
        "OMP_NUM_THREADS",
        "NSLOTS",
        "LSB_DJOB_NUMPROC",
        "SLURM_CPUS_PER_TASK",
    ]
    .iter()
    .find_map(|var| get_int_from_env(var))
}

/// The batch array-job index, from `LSB_JOBINDEX` (LSF) or `SGE_TASK_ID` (SGE), in that order
/// (mirrors `getJobIndex = getLSFjobIndex <|> getGEjobIndex`). Haskell reads it as an arbitrary
/// precision `Integer`; `i64` is more than enough for any real array-job index.
pub fn get_job_index() -> Option<i64> {
    get_int_from_env("LSB_JOBINDEX").or_else(|| get_int_from_env("SGE_TASK_ID"))
}

/// Read an environment variable and parse it as an integer, mirroring `(>>= readMaybe) <$>
/// lookupEnv`: an unset variable or an unparseable value both yield `None`.
fn get_int_from_env<T: std::str::FromStr>(var: &str) -> Option<T> {
    env::var(var).ok().and_then(|v| v.parse().ok())
}

#[cfg(test)]
mod tests {
    use super::*;

    // All `batch` environment variables, cleared around each assertion. These variables are not
    // touched by any other test, but env mutation is process-global, so everything runs inside one
    // test function to keep it deterministic regardless of test-thread scheduling.
    const VARS: &[&str] = &[
        "LSB_JOBINDEX",
        "SGE_TASK_ID",
        "OMP_NUM_THREADS",
        "NSLOTS",
        "LSB_DJOB_NUMPROC",
        "SLURM_CPUS_PER_TASK",
    ];

    fn clear() {
        for v in VARS {
            env::remove_var(v);
        }
    }

    #[test]
    fn ncpus_and_job_index() {
        // Save and restore so the test leaves the environment as it found it.
        let saved: Vec<(&str, Option<String>)> =
            VARS.iter().map(|v| (*v, env::var(v).ok())).collect();

        clear();
        assert_eq!(get_job_index(), None);
        assert_eq!(get_ncpus(), None);

        // LSF index, then SGE index, then LSF-wins-over-SGE precedence.
        env::set_var("SGE_TASK_ID", "3");
        assert_eq!(get_job_index(), Some(3));
        env::set_var("LSB_JOBINDEX", "9");
        assert_eq!(get_job_index(), Some(9));

        // Unparseable values are ignored (mirrors `readMaybe`).
        env::set_var("LSB_JOBINDEX", "abc");
        assert_eq!(get_job_index(), Some(3)); // falls back to SGE_TASK_ID
        env::remove_var("SGE_TASK_ID");
        assert_eq!(get_job_index(), None);

        clear();
        // getNcpus precedence: OMP_NUM_THREADS first, then the rest in order.
        env::set_var("SLURM_CPUS_PER_TASK", "4");
        assert_eq!(get_ncpus(), Some(4));
        env::set_var("NSLOTS", "8");
        assert_eq!(get_ncpus(), Some(8));
        env::set_var("OMP_NUM_THREADS", "2");
        assert_eq!(get_ncpus(), Some(2));
        env::set_var("OMP_NUM_THREADS", "not-a-number");
        assert_eq!(get_ncpus(), Some(8)); // falls through to NSLOTS

        clear();
        for (v, val) in saved {
            match val {
                Some(val) => env::set_var(v, val),
                None => env::remove_var(v),
            }
        }
    }
}
