//! Command-line script-execution flow, mirroring the `DefaultMode` path of `Execs/Main.hs`:
//! load → parse → version gate → type check → validate → interpret.
//!
//! Argument parsing is hand-rolled for the subset of flags currently used (notably by
//! `run-tests.sh`): `-n/--validate-only`, `-t/--temporary-directory`, `--quiet`,
//! `-v/--verbosity`, `--debug`, `--no-header`, and a single positional script path.

use crate::ast::NGLType;
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
    search_path: Vec<String>,
    /// `--subsample`: keep only a deterministic 1/10 of the reads on FASTQ load (mirrors
    /// `nConfSubsample`); also appends `.subsampled` to write outputs.
    subsample: bool,
    /// Positional arguments after the script path, exposed (with the script path) as `ARGV`.
    extra_args: Vec<String>,
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
            report_fatal_error(&e);
            1
        }
    }
}

/// Print a fatal error in the same format as `runNGLessIO` in `Execs/Main.hs`: a context line, a
/// category line for the error type, then the (red) message followed by a colour reset.
fn report_fatal_error(e: &crate::errors::NgError) {
    use crate::errors::NgErrorType::*;
    const RED: &str = "\u{1b}[31m";
    const RESET: &str = "\u{1b}[0m";
    eprintln!("Exiting after fatal error while loading and running script");
    match e.kind {
        ShouldNotOccur => eprintln!(
            "Should Not Occur Error! This probably indicates a bug in ngless.\n\
             \tPlease get in touch with the authors with a description of how this happened.\n\
             \tIf possible run your script with the --trace flag and post the script and the resulting trace at\n\
             \t\thttps://github.com/ngless-toolkit/ngless/issues.\n"
        ),
        ScriptError => eprintln!("Script Error (there is likely an error in your script)"),
        DataError => eprintln!("Data Error (the input data did not conform to NGLess' expectations)"),
        SystemError => {
            eprintln!("System Error (NGLess was not able to access some necessary resource)")
        }
        _ => {}
    }
    eprintln!("{RED}{e}");
    eprintln!("{RESET}");
}

fn parse_args(args: &[String]) -> NgResult<RunOpts> {
    let mut opts = RunOpts::default();
    let mut i = 0;
    while i < args.len() {
        let a = &args[i];
        match a.as_str() {
            "-n" | "--validate-only" => opts.validate_only = true,
            "--quiet" => opts.quiet = true,
            "--subsample" => opts.subsample = true,
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
            "--search-path" => {
                i += 1;
                opts.search_path.push(arg_value(args, i, a)?);
            }
            other if other.starts_with("--search-path=") => {
                opts.search_path
                    .push(other["--search-path=".len()..].to_string());
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
                } else {
                    opts.extra_args.push(a.clone());
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
    let temp_dir = opts
        .temp_dir
        .clone()
        .map(std::path::PathBuf::from)
        .unwrap_or_else(std::env::temp_dir);

    // Gather the functions contributed by imported modules. Built-in standard modules
    // (`samtools`/`mocat`/...) are handled by `module_functions`; everything else is loaded as an
    // external YAML module (`Modules/<name>.ngm/<version>/module.yaml`).
    let mut extra_funcs = Vec::new();
    let mut external_modules = Vec::new();
    let mut constants: Vec<(String, NGLType)> = Vec::new();
    let mut constant_values = Vec::new();
    for m in &header.modules {
        constants.extend(crate::modules::module_constants(m.name(), m.version()));
        constant_values.extend(crate::interpret::module_constant_values(
            m.name(),
            m.version(),
        ));
        match crate::modules::module_functions(m.name(), m.version()) {
            Some(fs) => extra_funcs.extend(fs),
            None => {
                let em = crate::external_modules::find_load(
                    m.name(),
                    m.version(),
                    &crate::interpret::data_directories(),
                )?;
                em.validate((version.major, version.minor), &temp_dir)?;
                extra_funcs.extend(em.functions_for_typecheck());
                external_modules.push(em);
            }
        }
    }

    if opts.debug == "ast" {
        for (lno, e) in &script.body {
            println!("{lno}: {e:?}");
        }
        return Ok(0);
    }

    // Type check, then validate.
    let typed = crate::types::checktypes(version, &script, &extra_funcs, &constants)?;
    let mut funcs = builtin_functions(version);
    funcs.extend(extra_funcs);
    let constant_names: Vec<String> = constants.iter().map(|(n, _)| n.clone()).collect();
    crate::validation::validate(&funcs, &constant_names, &typed)?;

    // IO validation (mirrors `validateIO`): run the count check for `count()` calls whose keyword
    // arguments are all statically known, so a non-existent feature column aborts before any output
    // is produced. Runs before the citation header is printed, matching Haskell's ordering.
    crate::validation::validate_count_io(&typed)?;

    if opts.validate_only {
        if !opts.quiet {
            eprintln!("Script OK.");
        }
        return Ok(0);
    }

    // The run header is suppressed by `--no-header` or when the script writes to STDOUT
    // (mirrors `nConfPrintHeader` and `setQuiet` on `uses_STDOUT`).
    if !opts.no_header && !crate::validation::uses_stdout(&typed) {
        let citations = crate::citations::collect_citations(&typed);
        crate::citations::print_header(&citations);
    }

    // Post-validation transforms (mirrors `Transform.transform`, run after `validate`). Inject
    // the `__hash` keyword argument into `write`/`collect` calls so `auto_comments=[{hash}]` can
    // report a content hash that is byte-identical to the Haskell build.
    let mut typed = typed;
    crate::transform::add_output_hash(
        &mut typed.body,
        (version.major, version.minor),
        &header.modules,
        &funcs,
    );
    // The parallel module contributes its own transform (`run_for_all`/`set_parallel_tag`/lock
    // hash). It runs after `add_output_hash`, mirroring Haskell's pre-transforms-then-module order.
    if let Some(parallel) = header.modules.iter().find(|m| m.name() == "parallel") {
        let include_for_all = parallel.version() == "1.1";
        crate::transform::parallel_transform(&mut typed.body, include_for_all)
            .map_err(NgError::script)?;
    }
    // Insert the floated `__check_ofile` calls (mirrors `addFileChecks`, a builtin transform that
    // runs after the module transforms). Done after output hashing so the inserted checks do not
    // affect the `{hash}`/`{script}` content hashes.
    typed.body = crate::transform::add_file_checks(std::mem::take(&mut typed.body), &funcs);

    // ARGV = [script_path, ...extra_args] (mirrors `nConfArgv` in Configuration.hs).
    let mut argv = vec![fname.clone()];
    argv.extend(opts.extra_args.iter().cloned());

    // Active mappers (mirrors `ngleMappersActive`): bwa is always available; importing the
    // `minimap2`/`soap` modules activates those mappers.
    let mut active_mappers = vec!["bwa".to_string()];
    for m in &header.modules {
        match m.name() {
            "minimap2" => active_mappers.push("minimap2".to_string()),
            "soap" => active_mappers.push("soap".to_string()),
            _ => {}
        }
    }

    crate::interpret::interpret(
        &typed.body,
        &temp_dir,
        &text,
        &opts.search_path,
        &argv,
        opts.subsample,
        external_modules,
        constant_values,
        active_mappers,
    )?;
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
