//! Command-line script-execution flow, mirroring the `DefaultMode` path of `Execs/Main.hs`:
//! load → parse → version gate → type check → validate → interpret.
//!
//! Argument parsing is hand-rolled for the subset of flags currently used (notably by
//! `run-tests.sh`): `-n/--validate-only`, `-t/--temporary-directory`, `--quiet`,
//! `-v/--verbosity`, `--debug`, `--no-header`, and a single positional script path.

use crate::errors::{NgError, NgResult};
use crate::modules::{builtin_functions, NGLVersion};

/// Minimum language version this build supports (the rewrite drops pre-1.5 semantics).
const MIN_VERSION: NGLVersion = NGLVersion { major: 1, minor: 5 };

#[derive(Default)]
struct RunOpts {
    script: Option<String>,
    validate_only: bool,
    quiet: bool,
    no_header: bool,
    debug: String,
    temp_dir: Option<String>,
}

/// Entry point for the non-informational command line (everything except `--version*` and
/// `--check-install`, which are handled in `lib::run`). Returns the process exit code.
pub fn run_default_mode(args: &[String]) -> i32 {
    let opts = match parse_args(args) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{e}");
            return 1;
        }
    };
    match run_script(&opts) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("{e}");
            1
        }
    }
}

fn parse_args(args: &[String]) -> NgResult<RunOpts> {
    let mut opts = RunOpts::default();
    let mut i = 0;
    while i < args.len() {
        let a = &args[i];
        match a.as_str() {
            "-n" | "--validate-only" => opts.validate_only = true,
            "--quiet" => opts.quiet = true,
            "--no-header" => opts.no_header = true,
            "-t" | "--temporary-directory" => {
                i += 1;
                opts.temp_dir = Some(arg_value(args, i, a)?);
            }
            "-v" | "--verbosity" => {
                i += 1;
                let _ = arg_value(args, i, a)?; // consumed but not yet used
            }
            "--debug" => {
                i += 1;
                opts.debug = arg_value(args, i, a)?;
            }
            other if other.starts_with("--temporary-directory=") => {
                opts.temp_dir = Some(other["--temporary-directory=".len()..].to_string());
            }
            other if other.starts_with("--debug=") => {
                opts.debug = other["--debug=".len()..].to_string();
            }
            other if other.starts_with('-') => {
                // Unknown flag: ignored for now (more flags will be wired up as the
                // corresponding features land).
            }
            _ => {
                if opts.script.is_none() {
                    opts.script = Some(a.clone());
                }
            }
        }
        i += 1;
    }
    Ok(opts)
}

fn arg_value(args: &[String], idx: usize, flag: &str) -> NgResult<String> {
    args.get(idx)
        .cloned()
        .ok_or_else(|| NgError::script(format!("Option {flag} requires a value")))
}

fn run_script(opts: &RunOpts) -> NgResult<i32> {
    let fname = opts
        .script
        .as_ref()
        .ok_or_else(|| NgError::script("No script file provided."))?;
    let text = std::fs::read_to_string(fname).map_err(|e| {
        NgError::new(
            crate::errors::NgErrorType::SystemError,
            format!("Could not read {fname}: {e}"),
        )
    })?;

    let script = crate::parser::parse_ngless(fname, true, &text)?;

    // Version gate: this build supports only ngless "1.5"+.
    let header = script
        .header
        .as_ref()
        .ok_or_else(|| NgError::script("Script is missing a version declaration."))?;
    let version = parse_version(&header.version).ok_or_else(|| {
        NgError::script(format!(
            "Could not parse ngless version '{}'.",
            header.version
        ))
    })?;
    if version < MIN_VERSION {
        return Err(NgError::script(format!(
            "Script declares ngless version \"{}\", but this build supports only ngless \"1.5\" and newer.\n\
             Update the version statement, or use the Haskell build for older scripts.",
            header.version
        )));
    }
    if !header.modules.is_empty() {
        return Err(NgError::script(
            "Module imports are not supported in this build yet.",
        ));
    }

    if opts.debug == "ast" {
        for (lno, e) in &script.body {
            println!("{lno}: {e:?}");
        }
        return Ok(0);
    }

    // Type check, then validate.
    let typed = crate::types::checktypes(version, &script)?;
    let funcs = builtin_functions(version);
    crate::validation::validate(&funcs, &[], &typed)?;

    if opts.validate_only {
        if !opts.quiet {
            eprintln!("Script OK.");
        }
        return Ok(0);
    }

    let temp_dir = opts
        .temp_dir
        .clone()
        .map(std::path::PathBuf::from)
        .unwrap_or_else(std::env::temp_dir);
    crate::interpret::interpret(&typed.body, &temp_dir)?;
    Ok(0)
}

fn parse_version(v: &str) -> Option<NGLVersion> {
    let (maj, rest) = split_digits(v);
    if maj.is_empty() || !rest.starts_with('.') {
        return None;
    }
    let (min, _) = split_digits(&rest[1..]);
    if min.is_empty() {
        return None;
    }
    Some(NGLVersion::new(maj.parse().ok()?, min.parse().ok()?))
}

fn split_digits(s: &str) -> (&str, &str) {
    let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
    (&s[..end], &s[end..])
}
