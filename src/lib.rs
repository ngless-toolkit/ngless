//! NGLess — NGS processing with Less work.
//!
//! This Rust crate is the implementation of NGLess. It replaced the original Haskell program,
//! which was removed at the 1.6 release (see `rust-migration.md` at the repo root for the port
//! history). The command-line entry point handles the informational flags (`--version`,
//! `--version-short`, `--version-debug`, `--date-short`), `--check-install` and `--print-path`,
//! and otherwise dispatches to [`cli`], which drives the full pipeline: load → tokenize
//! ([`tokens`]) → parse ([`parser`] → [`ast`]) → version gate → type check ([`types`]) →
//! validate ([`validation`]) → transform ([`transform`]) → interpret ([`interpret`]).
//!
//! The design goal is *behavioral parity* with the former Haskell implementation for
//! `ngless "1.5"`+ scripts: output must be byte-identical. This is verified against the
//! functional test suite under `tests/` via `NGLESS_BIN=<this binary> ./run-tests.sh` (all
//! tests pass), whose committed `expected.*` files were produced by the Haskell binary.

pub mod ast;
pub mod batch;
pub mod citations;
pub mod cleanup;
pub mod cli;
pub mod compression;
pub mod configuration;
pub mod count;
pub mod errors;
pub mod export;
pub mod external_modules;
pub mod fastq;
pub mod gff;
pub mod interpret;
pub mod lockfile;
pub mod mapper;
pub mod minimap2;
pub mod modules;
pub mod output;
pub mod parallel;
pub mod parser;
pub mod progress;
pub mod reference;
pub mod report;
pub mod sam;
pub mod samtools;
pub mod select;
pub mod suggestion;
pub mod tempfiles;
pub mod tokens;
pub mod transform;
pub mod types;
pub mod validation;
pub mod values;

/// Version strings and the `--version*` output format (originally modelled on `NGLess/Version.hs`
/// and `Execs/Main.hs`).
pub mod version {
    pub const VERSION_STR: &str = "1.6.0";
    pub const VERSION_STR_LONG: &str = "1.6.0";
    pub const DATE_STR: &str = "4 August 2026";
    // The Rust build does not yet embed external tool binaries (samtools/bwa/...).
    pub const EMBEDDED_STR: &str = "No";

    /// `--version` / `-V` output.
    pub fn version_line() -> String {
        format!("ngless v{VERSION_STR_LONG} (release date: {DATE_STR})")
    }

    /// `--version-debug` output.
    pub fn version_debug_line() -> String {
        format!(
            "ngless v{VERSION_STR} (full version: {VERSION_STR_LONG}; release date: {DATE_STR}; embedded binaries: {EMBEDDED_STR})"
        )
    }
}

/// Usage/help text, listing the flags this build actually supports (see `cli::parse_args`).
/// Used by `--help`/`-h` (printed to stdout) and as the usage message when no script is given
/// (printed to stderr by `cli::exec_default`).
pub(crate) fn help_text() -> String {
    format!(
        "{header}\n\
         \n\
         Usage: ngless [OPTIONS] SCRIPT [ARGS...]\n\
         \n\
         Run the ngless script SCRIPT. Any ARGS following it are exposed to the script as ARGV.\n\
         \n\
         Script:\n\
         \x20 -e, --script SCRIPT            Run an inline script instead of a script file\n\
         \x20 -p, --print-last               Print the value of the last expression to stdout\n\
         \x20 -n, --validate-only            Only validate the script; do not run it\n\
         \n\
         Resources:\n\
         \x20 -j, --jobs, --threads N        Number of threads to use\n\
         \x20     --strict-threads           Never use more threads than --jobs (even in bursts)\n\
         \x20     --no-strict-threads        Opposite of --strict-threads\n\
         \x20 -t, --temporary-directory DIR  Directory for temporary files\n\
         \x20     --keep-temporary-files     Do not delete temporary files when done\n\
         \x20     --no-keep-temporary-files  Opposite of --keep-temporary-files\n\
         \n\
         Paths:\n\
         \x20 -c, --config-file PATH         Configuration file to read (repeatable)\n\
         \x20     --search-path PATH         Add a search path for references/indices (repeatable)\n\
         \x20     --index-path PATH          Directory where mapper indices are stored\n\
         \n\
         Output:\n\
         \x20 -o, --html-report-directory D  Directory for the HTML run report\n\
         \x20     --create-report            Write an HTML run report (default for script files)\n\
         \x20     --no-create-report         Do not write an HTML run report\n\
         \x20 -v, --verbosity LEVEL          Set verbosity level (quiet|normal|full)\n\
         \x20 -q, --quiet                    Suppress informational output\n\
         \x20     --trace                    Highest verbosity mode (print all trace messages)\n\
         \x20     --no-trace                 Opposite of --trace\n\
         \x20     --no-header                Do not print the run header\n\
         \x20     --color WHEN               Colour output (auto|no|force|yes)\n\
         \n\
         Debugging:\n\
         \x20     --subsample                Subsample mode (process only a fraction of the data)\n\
         \x20     --debug MODE               Enable debug output (e.g. 'ast')\n\
         \n\
         Experimental (require --experimental-features):\n\
         \x20     --export-json FILE         Export the script as JSON\n\
         \x20     --export-cwl FILE          Generate a CWL wrapper for the script\n\
         \n\
         Data management (these do not run a script):\n\
         \x20     --install-reference-data N Download and install a builtin reference\n\
         \x20     --download-demo NAME       Download a demo dataset (gut-short, ocean-short)\n\
         \x20     --download-file            With --download-url URL --local-file PATH\n\
         \x20     --create-reference-pack    With --output-name, --genome-url and optionally\n\
         \x20                                --gtf-url and --functional-map-url\n\
         \n\
         Informational:\n\
         \x20 -V, --version                  Print version and exit\n\
         \x20     --version-short            Print short version string and exit\n\
         \x20     --version-debug            Print detailed version information and exit\n\
         \x20     --date-short               Print the release date and exit\n\
         \x20     --check-install            Verify the installation and exit (add --verbose\n\
         \x20                                to print the paths of the external tools)\n\
         \x20     --print-path EXEC          Print the resolved path to an external tool and exit\n\
         \x20 -h, --help                     Print this help message and exit\n\
         \n\
         Long options also accept --option=value. Short options may be bundled and joined to\n\
         their value (-nq, -j4, -nj4, -vfull, -pe 'ngless \"1.6\"; print(1)').\n\
         \n\
         ngless v{ver}(C) NGLess Authors 2013-2023\n\
         For more information:\n\
         \thttps://ngless.readthedocs.io\n\
         For comments/discussion:\n\
         \thttps://groups.google.com/forum/#!forum/ngless\n\
         Citation: LP Coelho et al., 2019. https://doi.org/10.1186/s40168-019-0684-8.",
        header = version::version_line(),
        ver = version::VERSION_STR_LONG,
    )
}

/// Handle a command line and return the process exit code.
///
/// `args` should be the arguments *after* the program name (i.e. `env::args().skip(1)`).
pub fn run<I, S>(args: I) -> i32
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let args: Vec<String> = args.into_iter().map(|s| s.as_ref().to_string()).collect();

    // Informational `infoOption`s, matching the Haskell CLI (Execs/Main.hs). These short-circuit at
    // any position, regardless of mode, so they are scanned before mode dispatch.
    for a in &args {
        match a.as_str() {
            "-V" | "--version" => {
                println!("{}", version::version_line());
                return 0;
            }
            "--version-short" => {
                println!("{}", version::VERSION_STR);
                return 0;
            }
            "--version-debug" => {
                println!("{}", version::version_debug_line());
                return 0;
            }
            "--date-short" => {
                println!("{}", version::DATE_STR);
                return 0;
            }
            "-h" | "--help" => {
                println!("{}", help_text());
                return 0;
            }
            _ => {}
        }
    }

    // Hidden developer aid: parse a script and report success/failure. Used to smoke-test the
    // front-end port against the real `tests/` scripts before the interpreter exists.
    if args.len() == 2 && args[0] == "--debug-parse" {
        return match std::fs::read_to_string(&args[1]) {
            Ok(src) => match parser::parse_ngless(&args[1], true, &src) {
                Ok(script) => {
                    println!("OK: {} top-level expressions", script.body.len());
                    0
                }
                Err(e) => {
                    eprintln!("PARSE ERROR: {e}");
                    1
                }
            },
            Err(e) => {
                eprintln!("could not read {}: {e}", args[1]);
                1
            }
        };
    }

    // Otherwise, dispatch on the execution mode (`CmdArgs.NGLessMode`): the default
    // load → parse → version-gate → type check → validate → interpret flow, plus the
    // `--print-path`/`--check-install` sub-modes (and the recognized-but-unimplemented ones).
    cli::run_cli(&args)
}

/// `--print-path EXEC`: print the path to the external tool `EXEC` (mirrors `PrintPathMode`
/// in `Execs/Main.hs`). The Rust build bundles no binaries, so the path is resolved from the
/// per-tool `NGLESS_*_BIN` environment variable or from `PATH`.
pub(crate) fn print_path(exec: &str) -> i32 {
    use errors::{NgError, NgErrorType};
    let resolved = match EXTERNAL_TOOLS.iter().find(|(name, _, _)| *name == exec) {
        Some((name, envvar, _)) => find_bin(envvar, name),
        None => Err(NgError::new(
            NgErrorType::SystemError,
            format!("Unknown binary {exec}."),
        )),
    };
    match resolved {
        Ok(path) => {
            println!("{path}");
            0
        }
        Err(e) => {
            eprintln!("{e}");
            1
        }
    }
}

/// Resolve the path to an external tool, mirroring `findNGLessBin`/`checkExecutable` in
/// `NGLess/FileManagement.hs`: honour the
/// `NGLESS_*_BIN` override (which must point at an executable file), otherwise look the tool
/// up on `PATH`.
fn find_bin(envvar: &str, fname: &str) -> errors::NgResult<String> {
    use errors::{NgError, NgErrorType};
    if let Ok(bin) = std::env::var(envvar) {
        let path = std::path::Path::new(&bin);
        if !path.is_file() {
            return Err(NgError::new(
                NgErrorType::SystemError,
                format!("{envvar} binary not found!\nExpected it at {bin}"),
            ));
        }
        if !is_executable(path) {
            return Err(NgError::new(
                NgErrorType::SystemError,
                format!("{envvar} binary found at {bin}.\nHowever, it is not an executable file!"),
            ));
        }
        return Ok(bin);
    }
    match find_on_path(fname) {
        Some(p) => Ok(p),
        None => Err(NgError::new(
            NgErrorType::SystemError,
            format!("Cannot find {fname} on the PATH (set {envvar} to point to its location)."),
        )),
    }
}

/// Search the directories in `$PATH` for an executable file named `fname`.
fn find_on_path(fname: &str) -> Option<String> {
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let candidate = dir.join(fname);
        if candidate.is_file() && is_executable(&candidate) {
            return Some(candidate.to_string_lossy().into_owned());
        }
    }
    None
}

/// Whether `path` is executable by the current process. On Unix this checks the executable
/// permission bits; on other platforms existence as a file is taken as sufficient.
fn is_executable(path: &std::path::Path) -> bool {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        match std::fs::metadata(path) {
            Ok(m) => m.permissions().mode() & 0o111 != 0,
            Err(_) => false,
        }
    }
    #[cfg(not(unix))]
    {
        path.is_file()
    }
}

/// External tools NGLess may invoke, with the environment variable that overrides each one's
/// location and whether `--check-install` requires it (minimap2 is optional: it is only needed for
/// `map(..., mapper='minimap2')`).
const EXTERNAL_TOOLS: [(&str, &str, bool); 5] = [
    ("samtools", "NGLESS_SAMTOOLS_BIN", true),
    ("bwa", "NGLESS_BWA_BIN", true),
    ("minimap2", "NGLESS_MINIMAP2_BIN", false),
    ("prodigal", "NGLESS_PRODIGAL_BIN", true),
    ("megahit", "NGLESS_MEGAHIT_BIN", true),
];

/// `--check-install [--verbose]`: check that the external tools can be found. A missing required
/// tool is an error (exit code 1); a missing optional one only triggers a warning. With
/// `--verbose`, the resolved path of each tool that is found is printed.
pub(crate) fn check_install(verbose: bool) -> i32 {
    check_tools(
        EXTERNAL_TOOLS
            .iter()
            .map(|&(name, envvar, required)| (name, required, find_bin(envvar, name))),
        verbose,
    )
}

/// Report on the lookup result for each `(tool, required, result)` and return the exit code.
fn check_tools<'a>(
    tools: impl Iterator<Item = (&'a str, bool, errors::NgResult<String>)>,
    verbose: bool,
) -> i32 {
    let mut ok = true;
    for (name, required, resolved) in tools {
        match resolved {
            Ok(path) => {
                if verbose {
                    println!("{name}: {path}");
                }
            }
            Err(e) => {
                let msg = e.message.replace('\n', " ");
                if required {
                    eprintln!("Error: {msg}");
                    ok = false;
                } else {
                    eprintln!("Warning: {msg} {name} is optional, so the check still passes.");
                }
            }
        }
    }
    if ok {
        println!("Install OK");
        0
    } else {
        eprintln!("Install check FAILED: required external tools are missing.");
        1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn version_short_is_plain_string() {
        assert_eq!(version::VERSION_STR, "1.6.0");
    }

    #[test]
    fn version_line_matches_haskell_format() {
        assert_eq!(
            version::version_line(),
            "ngless v1.6.0 (release date: 4 August 2026)"
        );
    }

    #[test]
    fn version_debug_mentions_embedded() {
        assert!(version::version_debug_line().contains("embedded binaries: No"));
    }

    #[test]
    fn version_flag_exits_zero() {
        assert_eq!(run(["--version"]), 0);
        assert_eq!(run(["-V"]), 0);
        assert_eq!(run(["--version-debug"]), 0);
    }

    #[test]
    fn check_install_requires_tools() {
        let found = |name: &'static str, required| (name, required, Ok(format!("/bin/{name}")));
        let missing = |name: &'static str, required| {
            (name, required, Err(errors::NgError::script("missing")))
        };
        assert_eq!(check_tools([found("bwa", true)].into_iter(), true), 0);
        // A missing optional tool (minimap2) is only a warning.
        assert_eq!(
            check_tools(
                [found("bwa", true), missing("minimap2", false)].into_iter(),
                false
            ),
            0
        );
        assert_eq!(
            check_tools(
                [missing("bwa", true), found("minimap2", false)].into_iter(),
                true
            ),
            1
        );
    }

    #[test]
    fn print_path_unknown_binary_exits_nonzero() {
        assert_eq!(run(["--print-path", "no-such-tool"]), 1);
    }

    #[test]
    fn print_path_env_override_is_resolved() {
        // Point a tool at a known executable via its override variable and check it round-trips.
        let me = std::env::current_exe().unwrap();
        std::env::set_var("NGLESS_SAMTOOLS_BIN", &me);
        let resolved = find_bin("NGLESS_SAMTOOLS_BIN", "samtools").unwrap();
        std::env::remove_var("NGLESS_SAMTOOLS_BIN");
        assert_eq!(resolved, me.to_string_lossy());
    }

    #[test]
    fn running_a_script_is_not_implemented_yet() {
        assert_eq!(run(["script.ngl"]), 1);
    }
}
