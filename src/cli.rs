//! Command-line script-execution flow, mirroring the `DefaultMode` path of `Execs/Main.hs`:
//! load → parse → version gate → type check → validate → interpret.
//!
//! Argument parsing is hand-rolled for the subset of flags currently used (notably by
//! `run-tests.sh`): `-n/--validate-only`, `-t/--temporary-directory`,
//! `--keep-temporary-files`, `-q/--quiet`, `-v/--verbosity`, `--trace`, `--debug`,
//! `--no-header`, and a single positional script path.

use crate::ast::NGLType;
use crate::errors::{NgError, NgResult};
use crate::modules::{builtin_functions, NGLVersion};
use crate::output::{self, ColorSetting, Verbosity};

/// Oldest language version this build still accepts (with a deprecation warning). The rewrite
/// drops pre-1.5 semantics, so anything older is a hard error.
const MIN_VERSION: NGLVersion = NGLVersion { major: 1, minor: 5 };

/// The current language version this (Rust) build implements natively. Scripts should declare
/// this going forward.
const CURRENT_VERSION: NGLVersion = NGLVersion { major: 1, minor: 6 };

/// The effective semantics version used for feature gating and output hashing. Version 1.6 is a
/// best-effort exact mirror of 1.5, so both 1.5 and 1.6 map to these semantics (mirroring the
/// Haskell `parseVersion (Just "1.6") = NGLVersion 1 5`). This keeps output — including the
/// `{hash}` auto-comment — byte-identical between a 1.5 and a 1.6 run.
const EFFECTIVE_VERSION: NGLVersion = NGLVersion { major: 1, minor: 5 };

#[derive(Default)]
struct RunOpts {
    script: Option<String>,
    validate_only: bool,
    quiet: bool,
    /// `-v/--verbosity quiet|normal|full` (mirrors `parseVerbosity`). Defaults to `Normal`.
    verbosity: Verbosity,
    /// `--trace`: highest verbosity mode — print all levels, including `Trace` (mirrors
    /// `trace_flag`/`nConfTrace`).
    trace: bool,
    no_header: bool,
    debug: String,
    temp_dir: Option<String>,
    /// `--keep-temporary-files`/`--no-keep-temporary-files`: whether to delete temporary files at
    /// the end of the run (mirrors the `keep_temporary_files` triSwitch → `nConfKeepTemporaryFiles`).
    /// `None` falls through to the config value.
    keep_temporary_files: Option<bool>,
    search_path: Vec<String>,
    /// `--subsample`: keep only a deterministic 1/10 of the reads on FASTQ load (mirrors
    /// `nConfSubsample`); also appends `.subsampled` to write outputs.
    subsample: bool,
    /// `-j/--jobs`/`--threads`: number of threads for NGLess's own parallel work and for the
    /// thread count handed to external tools. `0` means "unset" and resolves to 1 (the Haskell
    /// default, `NThreads 1`); `auto` resolves to the detected CPU count at parse time.
    n_threads: usize,
    /// `--strict-threads`/`--no-strict-threads`: strictly respect `--jobs` by reserving one core
    /// for NGLess/system work when invoking external mappers (mirrors the `strict-threads`
    /// triSwitch → `nConfStrictThreads`). `None` falls through to the config value.
    strict_threads: Option<bool>,
    /// `-c/--config-file`: extra configuration files to read (required to exist), applied after the
    /// default locations so they take precedence (mirrors `config_files`).
    config_files: Vec<String>,
    /// `--index-path`: where to store mapper indices (mirrors `nConfIndexStorePath`); when set, it
    /// overrides the config-file `index-path`.
    index_path: Option<String>,
    /// `-e/--script`: inline script source (mirrors `InlineScript`). When set there is no version
    /// requirement and the script name is `"inline"` (and is not prepended to `ARGV`).
    inline_script: Option<String>,
    /// `-p/--print-last`: wrap the last statement in `write(<expr>, ofile=STDOUT)` (mirrors
    /// `wrapPrint`/`print_last`).
    print_last: bool,
    /// `--color auto|no|yes|force`: colour override (mirrors `parseColor`). `None` falls through to
    /// the config value; otherwise it overrides the config-file `color` key.
    color: Option<ColorSetting>,
    /// `--experimental-features`: gate for the export modes (mirrors `experimentalFeatures`; its
    /// *only* effect in Haskell). Parsed here; consumed by Phase 3.
    experimental_features: bool,
    /// `--export-json FILE`/`--export-cwl FILE`: serialise the transformed script (mirrors
    /// `exportJSON`/`exportCWL`). Parsed here; implemented in Phase 3.
    export_json: Option<String>,
    export_cwl: Option<String>,
    /// `--check-deprecation`: check whether the ngless version or any used modules are deprecated
    /// (mirrors `deprecationCheck`). Parsed here; implemented in a later phase.
    deprecation_check: bool,
    /// `--create-report`/`--no-create-report`: whether to write the HTML report directory (mirrors
    /// the `createReportDirectory` triSwitch). `None` falls through to the config value.
    create_report: Option<bool>,
    /// `-o/--html-report-directory DIR`: HTML report output directory (mirrors
    /// `html_report_directory`). Parsed here; consumed once the HTML report lands.
    html_report_directory: Option<String>,
    /// Positional arguments after the script path, exposed (with the script path) as `ARGV`.
    extra_args: Vec<String>,
}

/// Top-level execution mode, mirroring `CmdArgs.NGLessMode`. `lib::run` peels off the
/// informational `infoOption`s (`--version*`/`--date-short`/`--help`) first; everything else is
/// dispatched here. All modes — [`Mode::Default`], [`Mode::PrintPath`], [`Mode::CheckInstall`], the
/// download sub-modes, and [`Mode::CreateReferencePack`] — are implemented.
enum Mode {
    /// `DefaultMode`: load → parse → type check → validate → interpret a script.
    Default(Box<RunOpts>),
    /// `--print-path EXEC` (`PrintPathMode`).
    PrintPath(String),
    /// `--check-install` (`CheckInstallMode`).
    CheckInstall,
    /// `--install-reference-data REF` (`InstallReferenceMode`).
    InstallReferenceData(String),
    /// `--download-file --download-url URL --local-file PATH` (`DownloadFileMode`).
    DownloadFile { url: String, local: String },
    /// `--download-demo NAME` (`DownloadDemoMode`).
    DownloadDemo(String),
    /// `--create-reference-pack --output-name NAME --genome-url URL [--gtf-url URL]
    /// [--functional-map-url URL]` (`CreateReferencePackMode`).
    CreateReferencePack {
        output_name: String,
        genome_url: String,
        gtf_url: Option<String>,
        functional_map_url: Option<String>,
    },
}

/// Entry point for the non-informational command line (everything except the `infoOption`s
/// `--version*`/`--date-short`/`-h/--help`, which short-circuit in `lib::run`). Dispatches on the
/// execution mode (`CmdArgs.NGLessMode`) and returns the process exit code.
pub fn run_cli(args: &[String]) -> i32 {
    let mode = match parse_mode(args) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("{e}");
            return 1;
        }
    };
    match mode {
        Mode::Default(opts) => exec_default(&opts),
        Mode::PrintPath(exec) => crate::print_path(&exec),
        Mode::CheckInstall => crate::check_install(),
        Mode::InstallReferenceData(refname) => exec_install_reference(&refname),
        Mode::DownloadFile { url, local } => exec_download_file(&url, &local),
        Mode::DownloadDemo(name) => exec_download_demo(&name),
        Mode::CreateReferencePack {
            output_name,
            genome_url,
            gtf_url,
            functional_map_url,
        } => exec_create_reference_pack(
            &output_name,
            &genome_url,
            gtf_url.as_deref(),
            functional_map_url.as_deref(),
        ),
    }
}

/// Read a required named option (`--name VALUE` or `--name=VALUE`) from the raw argument list. Used
/// by the download sub-modes, whose options are plain `strOption`s (no `=`-vs-space distinction in
/// optparse).
fn named_option(args: &[String], name: &str) -> NgResult<String> {
    let prefix = format!("{name}=");
    let mut it = args.iter();
    while let Some(a) = it.next() {
        if a == name {
            return it
                .next()
                .cloned()
                .ok_or_else(|| NgError::script(format!("Option {name} requires a value")));
        }
        if let Some(v) = a.strip_prefix(&prefix) {
            return Ok(v.to_string());
        }
    }
    Err(NgError::script(format!("Missing required option {name}")))
}

/// Read an optional named option (`--name VALUE` or `--name=VALUE`); returns `None` when absent
/// (mirrors `optional (strOption ...)`).
fn named_option_opt(args: &[String], name: &str) -> Option<String> {
    named_option(args, name).ok()
}

/// Initialise the process-global configuration for the non-`Default` sub-modes, mirroring
/// `initConfiguration` in `main'` (which runs before every `modeExec`). The download sub-modes need
/// the resolved `download-url`/data-directory configuration; they take no `-c/--config-file`, so
/// only the default config-file locations are read.
fn init_submode_config() -> NgResult<()> {
    let config = crate::configuration::read_config_files(&[])?;
    crate::configuration::init_global(config);
    Ok(())
}

/// `--install-reference-data REF` (`InstallReferenceMode`): download and install a builtin
/// reference. Mirrors `modeExec (InstallReferenceMode ref)`: a non-builtin reference is rejected,
/// otherwise the data is installed at the version-`Nothing` default version (1.5).
fn exec_install_reference(refname: &str) -> i32 {
    if let Err(e) = init_submode_config() {
        report_fatal_error(&e);
        return 1;
    }
    if !crate::reference::is_builtin_reference(refname) {
        // Mirrors the Haskell `error (concat ["Reference ", ref, " is not a known reference."])`.
        eprintln!("Reference {refname} is not a known reference.");
        return 1;
    }
    // `parseVersion Nothing` is 1.5, the version used to build the download URL.
    match crate::reference::install_data(refname, (1, 5)) {
        Ok(_) => 0,
        Err(e) => {
            report_fatal_error(&e);
            1
        }
    }
}

/// `--download-file --download-url URL --local-file PATH` (`DownloadFileMode`): download a single
/// file to a local path. Mirrors `modeExec (DownloadFileMode url local)`.
fn exec_download_file(url: &str, local: &str) -> i32 {
    if let Err(e) = init_submode_config() {
        report_fatal_error(&e);
        return 1;
    }
    match crate::reference::download_file(url, std::path::Path::new(local)) {
        Ok(()) => 0,
        Err(e) => {
            report_fatal_error(&e);
            1
        }
    }
}

/// `--download-demo NAME` (`DownloadDemoMode`): download and unpack a demo dataset into the current
/// directory. Mirrors `modeExec (DownloadDemoMode demo)`, including the known-demo list and the
/// "Unknown demo" suggestion message.
fn exec_download_demo(name: &str) -> i32 {
    const KNOWN: [&str; 2] = ["gut-short", "ocean-short"];
    if !KNOWN.contains(&name) {
        // Reproduce the Haskell stderr block verbatim: red "Unknown demo", the suggestion message,
        // an "Available demos are:" header, the tab-bulleted list, then a colour reset.
        const RED: &str = "\u{1b}[31m";
        const RESET: &str = "\u{1b}[0m";
        let suggestion = crate::suggestion::suggestion_message(name, &KNOWN);
        eprintln!("{RED}Unknown demo '{name}'.\n{suggestion}Available demos are:");
        for d in KNOWN {
            eprintln!("\t- {d}");
        }
        eprintln!("{RESET}");
        return 1;
    }
    if let Err(e) = init_submode_config() {
        report_fatal_error(&e);
        return 1;
    }
    let base = crate::reference::download_base_url();
    let base = base.trim_end_matches('/');
    let url = format!("{base}/Demos/{name}.tar.gz");
    match crate::reference::download_expand_tar(&url, std::path::Path::new(".")) {
        Ok(()) => {
            println!("\nDemo downloaded to {name}");
            0
        }
        Err(e) => {
            report_fatal_error(&e);
            1
        }
    }
}

/// `--create-reference-pack ...` (`CreateReferencePackMode`): download/copy a genome (and optional
/// GTF/functional-map), build a bwa index and pack everything into a gzipped tar. Mirrors
/// `modeExec (CreateReferencePackMode ofile gen mgtf mfunc)`.
fn exec_create_reference_pack(
    output_name: &str,
    genome_url: &str,
    gtf_url: Option<&str>,
    functional_map_url: Option<&str>,
) -> i32 {
    if let Err(e) = init_submode_config() {
        report_fatal_error(&e);
        return 1;
    }
    // Diagnostics from `create_reference_pack` (the "Starting packaging..."/"Created..." lines) go
    // through the output layer, so initialise it at the default verbosity.
    let config = crate::configuration::global();
    output::init(Verbosity::Normal, false, false, config.color);
    match crate::reference::create_reference_pack(
        output_name,
        genome_url,
        gtf_url,
        functional_map_url,
    ) {
        Ok(()) => 0,
        Err(e) => {
            report_fatal_error(&e);
            1
        }
    }
}

/// Run `DefaultMode`: load → parse → version gate → type check → validate → interpret.
fn exec_default(opts: &RunOpts) -> i32 {
    // Install signal-driven removal of lock/temporary files before any worker threads are spawned,
    // so an interrupted run (Ctrl+C / SIGTERM) does not leave stale artifacts behind.
    crate::cleanup::install();
    // No script to run: print the usage message (to stderr) rather than the generic
    // fatal-error block, mirroring optparse-applicative's behavior on a missing argument. An
    // inline script (`-e/--script`) counts as a script source even without a positional.
    if opts.script.is_none() && opts.inline_script.is_none() {
        eprintln!("{}", crate::help_text());
        return 1;
    }
    match run_script(opts) {
        Ok(code) => code,
        Err(e) => {
            report_fatal_error(&e);
            1
        }
    }
}

/// Determine the execution mode (`CmdArgs.nglessArgs`). The sub-modes are each selected by a unique
/// long flag (`--print-path`/`--check-install`/`--install-reference-data`/`--download-file`/
/// `--download-demo`/`--create-reference-pack`); their presence is unambiguous, so we route on it
/// directly rather than reproducing optparse's `<|>` backtracking. Anything else is `DefaultMode`.
fn parse_mode(args: &[String]) -> NgResult<Mode> {
    if let Some(pos) = args.iter().position(|a| a == "--print-path") {
        // `--print-path` is a bare flag taking the EXEC name as the following argument (mirrors
        // `printPathArgs`'s positional `strArgument`).
        let exec = args
            .get(pos + 1)
            .cloned()
            .ok_or_else(|| NgError::script("--print-path requires an argument (EXEC)"))?;
        return Ok(Mode::PrintPath(exec));
    }
    if args.iter().any(|a| a == "--check-install") {
        return Ok(Mode::CheckInstall);
    }
    if let Some(pos) = args.iter().position(|a| a == "--install-reference-data") {
        // The reference name is a positional argument (mirrors `installArgs`'s `strArgument REF`).
        let refname = args.get(pos + 1).cloned().ok_or_else(|| {
            NgError::script("--install-reference-data requires an argument (REF)")
        })?;
        return Ok(Mode::InstallReferenceData(refname));
    }
    if args.iter().any(|a| a == "--download-file") {
        // `--download-file` takes two named options, `--download-url` and `--local-file` (mirrors
        // `downloadFileArgs`'s two `strOption`s).
        let url = named_option(args, "--download-url")?;
        let local = named_option(args, "--local-file")?;
        return Ok(Mode::DownloadFile { url, local });
    }
    if let Some(pos) = args.iter().position(|a| a == "--download-demo") {
        // The demo name is a positional argument (mirrors `downloadDemoArgs`'s `strArgument`).
        let name = args
            .get(pos + 1)
            .cloned()
            .ok_or_else(|| NgError::script("--download-demo requires an argument (DEMO-NAME)"))?;
        return Ok(Mode::DownloadDemo(name));
    }
    if args.iter().any(|a| a == "--create-reference-pack") {
        // `--output-name`/`--genome-url` are required `strOption`s; `--gtf-url`/`--functional-map-url`
        // are optional (mirrors `createRefArgs`).
        let output_name = named_option(args, "--output-name")?;
        let genome_url = named_option(args, "--genome-url")?;
        let gtf_url = named_option_opt(args, "--gtf-url");
        let functional_map_url = named_option_opt(args, "--functional-map-url");
        return Ok(Mode::CreateReferencePack {
            output_name,
            genome_url,
            gtf_url,
            functional_map_url,
        });
    }
    Ok(Mode::Default(Box::new(parse_args(args)?)))
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
            "-q" | "--quiet" => opts.quiet = true,
            "--trace" => opts.trace = true,
            "--no-trace" => opts.trace = false,
            "--subsample" => opts.subsample = true,
            // triSwitch flags: `--flag`/`--no-flag` set `Some(true)`/`Some(false)`; absent stays
            // `None` and falls through to the config value (mirrors `triSwitch`/`fromMaybe`).
            "--strict-threads" => opts.strict_threads = Some(true),
            "--no-strict-threads" => opts.strict_threads = Some(false),
            "--keep-temporary-files" => opts.keep_temporary_files = Some(true),
            "--no-keep-temporary-files" => opts.keep_temporary_files = Some(false),
            "--create-report" => opts.create_report = Some(true),
            "--no-create-report" => opts.create_report = Some(false),
            "-p" | "--print-last" => opts.print_last = true,
            "--experimental-features" => opts.experimental_features = true,
            "--check-deprecation" => opts.deprecation_check = true,
            "-e" | "--script" => {
                i += 1;
                opts.inline_script = Some(arg_value(args, i, a)?);
            }
            other if other.starts_with("--script=") => {
                opts.inline_script = Some(other["--script=".len()..].to_string());
            }
            "--color" => {
                i += 1;
                opts.color = Some(parse_color(&arg_value(args, i, a)?)?);
            }
            other if other.starts_with("--color=") => {
                opts.color = Some(parse_color(&other["--color=".len()..])?);
            }
            "--export-json" => {
                i += 1;
                opts.export_json = Some(arg_value(args, i, a)?);
            }
            other if other.starts_with("--export-json=") => {
                opts.export_json = Some(other["--export-json=".len()..].to_string());
            }
            "--export-cwl" => {
                i += 1;
                opts.export_cwl = Some(arg_value(args, i, a)?);
            }
            other if other.starts_with("--export-cwl=") => {
                opts.export_cwl = Some(other["--export-cwl=".len()..].to_string());
            }
            "-o" | "--html-report-directory" => {
                i += 1;
                opts.html_report_directory = Some(arg_value(args, i, a)?);
            }
            other if other.starts_with("--html-report-directory=") => {
                opts.html_report_directory =
                    Some(other["--html-report-directory=".len()..].to_string());
            }
            // `--search-dir` is a deprecated alias for `--search-path` (mirrors `searchDir`).
            "--search-dir" => {
                i += 1;
                opts.search_path.push(arg_value(args, i, a)?);
            }
            other if other.starts_with("--search-dir=") => {
                opts.search_path
                    .push(other["--search-dir=".len()..].to_string());
            }
            "-j" | "--jobs" | "--threads" => {
                i += 1;
                opts.n_threads = parse_jobs(&arg_value(args, i, a)?)?;
            }
            other if other.starts_with("--jobs=") => {
                opts.n_threads = parse_jobs(&other["--jobs=".len()..])?;
            }
            other if other.starts_with("--threads=") => {
                opts.n_threads = parse_jobs(&other["--threads=".len()..])?;
            }
            "--no-header" => opts.no_header = true,
            "-t" | "--temporary-directory" => {
                i += 1;
                opts.temp_dir = Some(arg_value(args, i, a)?);
            }
            "-v" | "--verbosity" => {
                i += 1;
                opts.verbosity = parse_verbosity(&arg_value(args, i, a)?)?;
            }
            other if other.starts_with("--verbosity=") => {
                opts.verbosity = parse_verbosity(&other["--verbosity=".len()..])?;
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
            "-c" | "--config-file" => {
                i += 1;
                opts.config_files.push(arg_value(args, i, a)?);
            }
            other if other.starts_with("--config-file=") => {
                opts.config_files
                    .push(other["--config-file=".len()..].to_string());
            }
            "--index-path" => {
                i += 1;
                opts.index_path = Some(arg_value(args, i, a)?);
            }
            other if other.starts_with("--index-path=") => {
                opts.index_path = Some(other["--index-path=".len()..].to_string());
            }
            other if other.starts_with("--temporary-directory=") => {
                opts.temp_dir = Some(other["--temporary-directory=".len()..].to_string());
            }
            other if other.starts_with("--debug=") => {
                opts.debug = other["--debug=".len()..].to_string();
            }
            other if other.starts_with('-') && other != "-" => {
                // Unknown flag: error out, mirroring optparse-applicative (`-` alone is a valid
                // positional meaning STDIN, so it is *not* treated as a flag).
                return Err(NgError::script(format!("Invalid option `{other}'")));
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

/// Parse a `--color` value (mirrors `parseColor`/`readColor`): `auto`/`no`/`yes`/`force`.
fn parse_color(s: &str) -> NgResult<ColorSetting> {
    match s {
        "auto" => Ok(ColorSetting::Auto),
        "no" => Ok(ColorSetting::No),
        "yes" | "force" => Ok(ColorSetting::Force),
        _ => Err(NgError::script(
            "Could not parse color option (valid options are 'auto', 'force', and 'no')",
        )),
    }
}

/// Parse a `-v/--verbosity` value (mirrors `readVerbosity`: `quiet`/`normal`/`full`).
fn parse_verbosity(s: &str) -> NgResult<Verbosity> {
    match s {
        "quiet" => Ok(Verbosity::Quiet),
        "normal" => Ok(Verbosity::Normal),
        "full" => Ok(Verbosity::Loud),
        other => Err(NgError::script(format!(
            "Cannot parse '{other}' as a verbosity (expected 'quiet', 'normal' or 'full')."
        ))),
    }
}

/// Emit the resolved configuration as `Debug`-level messages, mirroring `outputConfiguration`
/// (`Output.hs`). These are visible only under `--trace`. The strings reproduce Haskell's derived
/// `Show` for `Bool` (`True`/`False`), `ColorSetting` (`AutoColor`/`NoColor`/`ForceColor`) and
/// `Verbosity` (`Quiet`/`Normal`/`Loud`).
fn output_configuration(config: &crate::configuration::Configuration, opts: &RunOpts) {
    let color = match config.color {
        // For `AutoColor`, also report what the automatic resolution (terminal status + `NO_COLOR`)
        // decided, since the effective behaviour is not visible from the bare setting name.
        ColorSetting::Auto => {
            let resolved = if config.color.resolve() {
                "color"
            } else {
                "no color"
            };
            format!("AutoColor ({resolved})")
        }
        ColorSetting::No => "NoColor".to_string(),
        ColorSetting::Force => "ForceColor".to_string(),
    };
    // `--quiet` forces `Quiet`, mirroring `if quiet then Quiet else verbosity`.
    let verbosity = if opts.quiet {
        Verbosity::Quiet
    } else {
        opts.verbosity
    };
    let verbosity = match verbosity {
        Verbosity::Quiet => "Quiet",
        Verbosity::Normal => "Normal",
        Verbosity::Loud => "Loud",
    };
    let show_bool = |b: bool| if b { "True" } else { "False" };
    output::debug(0, "# Configuration");
    output::debug(
        0,
        &format!("\tdownload base URL: {}", config.download_base_url),
    );
    output::debug(
        0,
        &format!("\tglobal data directory: {}", config.global_data_directory),
    );
    output::debug(0, &format!("\tuser directory: {}", config.user_directory));
    output::debug(
        0,
        &format!("\tuser data directory: {}", config.user_data_directory),
    );
    output::debug(
        0,
        &format!("\ttemporary directory: {}", config.temporary_directory),
    );
    output::debug(
        0,
        &format!(
            "\tkeep temporary files: {}",
            show_bool(config.keep_temporary_files)
        ),
    );
    output::debug(
        0,
        &format!(
            "\tcreate report: {}",
            show_bool(config.create_report_directory)
        ),
    );
    // `nConfReportDirectory` has no config-file key and defaults to the empty string (it is only
    // ever set by `-o/--html-report-directory`, which is not yet ported).
    output::debug(0, "\treport directory: ");
    output::debug(0, &format!("\tcolor setting: {color}"));
    output::debug(
        0,
        &format!("\tprint header: {}", show_bool(config.print_header)),
    );
    output::debug(0, &format!("\tsubsample: {}", show_bool(opts.subsample)));
    output::debug(0, &format!("\tverbosity: {verbosity}"));
    if let Some(p) = &config.index_store_path {
        output::debug(0, &format!("\tindex storage path: {p}"));
    }
    output::debug(0, "\tsearch path:");
    for p in &config.search_path {
        output::debug(0, &format!("\t\t{p}"));
    }
}

/// Parse a `-j/--jobs`/`--threads` value (mirrors `NThreadsOpts`): `auto` resolves to the
/// detected CPU count; otherwise a positive integer. `0` is rejected.
fn parse_jobs(s: &str) -> NgResult<usize> {
    if s == "auto" {
        return Ok(std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1));
    }
    match s.parse::<usize>() {
        Ok(n) if n >= 1 => Ok(n),
        _ => Err(NgError::script(format!(
            "Cannot parse '{s}' as a thread count (expected 'auto' or a positive integer)."
        ))),
    }
}

/// Read a script file as UTF-8 (mirrors the file/STDIN branches of `loadScript`). A bare `-` reads
/// the script from standard input.
fn load_script_file(fname: &str) -> NgResult<String> {
    use crate::errors::NgErrorType::SystemError;
    if fname == "-" {
        use std::io::Read;
        let mut s = String::new();
        std::io::stdin().read_to_string(&mut s).map_err(|e| {
            NgError::new(
                SystemError,
                format!("Could not read script from STDIN: {e}"),
            )
        })?;
        Ok(s)
    } else {
        std::fs::read_to_string(fname)
            .map_err(|e| NgError::new(SystemError, format!("Could not read {fname}: {e}")))
    }
}

fn arg_value(args: &[String], idx: usize, flag: &str) -> NgResult<String> {
    args.get(idx)
        .cloned()
        .ok_or_else(|| NgError::script(format!("Option {flag} requires a value")))
}

fn run_script(opts: &RunOpts) -> NgResult<i32> {
    // The export modes are gated on `--experimental-features` (mirrors the gate at the top of
    // `modeExec DefaultMode`: its *only* effect in Haskell).
    if !opts.experimental_features {
        if opts.export_json.is_some() {
            return Err(NgError::script(
                "The use of --export-json requires the --experimental-features flag\n\
                 This feature may change at any time.\n",
            ));
        }
        if opts.export_cwl.is_some() {
            return Err(NgError::script(
                "The use of --export-cwl requires the --experimental-features flag\n\
                 This feature may change at any time.\n",
            ));
        }
    }
    // Build the configuration in the three Haskell steps (`initConfiguration`): environment
    // defaults (`guessConfiguration`), config files (`readConfigFiles`), then command-line
    // overrides (`updateConfigurationOpts`). The triSwitch flags can turn an option on *or* off; an
    // absent flag (`None`) falls through to the config-file value (mirrors `fromMaybe`).
    let mut config = crate::configuration::read_config_files(&opts.config_files)?;
    if let Some(b) = opts.keep_temporary_files {
        config.keep_temporary_files = b;
    }
    if let Some(b) = opts.strict_threads {
        config.strict_threads = b;
    }
    if let Some(t) = &opts.temp_dir {
        config.temporary_directory = t.clone();
    }
    if !opts.search_path.is_empty() {
        config.search_path = opts.search_path.clone();
    }
    if opts.index_path.is_some() {
        config.index_store_path = opts.index_path.clone();
    }
    // `--color` overrides the config-file `color` key (mirrors `color <|> nConfColor`). An absent
    // flag (`None`) falls through to the config value.
    if let Some(c) = opts.color {
        config.color = c;
    }
    // `--no-header` also disables the banner (mirrors `nConfPrintHeader && not no_header`).
    if opts.no_header {
        config.print_header = false;
    }
    // Publish the resolved configuration process-globally so the download/data-directory helpers
    // (`crate::reference`) read the config-file values (mirrors the global `nglConfiguration`).
    crate::configuration::init_global(config.clone());

    // Configure the global output layer before any diagnostics are emitted (mirrors building
    // `nglConfiguration` from the parsed args). `--quiet` overrides `-v`; `--trace` forces the
    // highest verbosity; `color` comes from the config file.
    output::init(opts.verbosity, opts.quiet, opts.trace, config.color);
    // Dump the resolved configuration as `Debug`-level messages (shown only under `--trace`),
    // mirroring `outputConfiguration` in `Output.hs`.
    output_configuration(&config, &opts);
    // `n_threads == 0` means the flag was not given: default to a single thread (the Haskell
    // default), which keeps output byte-for-byte reproducible unless the user opts in.
    let n_threads = if opts.n_threads == 0 {
        1
    } else {
        opts.n_threads
    };
    crate::parallel::init(n_threads, config.strict_threads);

    // Determine the script source (mirrors `loadScript` + the `(fname, reqversion)` split in
    // `modeExec DefaultMode`): an inline script (`-e/--script`) is named `"inline"` and carries no
    // version requirement; a file (or `-` for STDIN) is read as UTF-8 and *does* require a version
    // declaration.
    let (fname, text, req_version) = match &opts.inline_script {
        Some(src) => ("inline".to_string(), src.clone(), false),
        None => {
            let fname = opts
                .script
                .as_ref()
                .ok_or_else(|| NgError::script("No script file provided."))?;
            (fname.clone(), load_script_file(fname)?, true)
        }
    };

    let mut script = crate::parser::parse_ngless(&fname, req_version, &text)?;

    // `-p/--print-last`: wrap the last statement in `write(<expr>, ofile=STDOUT)`, mirroring
    // `wrapPrint` applied right after `parsengless`. Done before type-checking so the injected
    // `write` is type-checked, validated and seen by `uses_STDOUT` (header suppression).
    if opts.print_last {
        crate::transform::wrap_print(&mut script.body).map_err(NgError::script)?;
    }

    // Version gate. This build implements ngless "1.6" natively. Version 1.6 is a best-effort
    // exact mirror of 1.5, so both are accepted and share the same `EFFECTIVE_VERSION` semantics;
    // declaring 1.5 additionally emits a deprecation warning (the project is now based on the Rust
    // implementation and 1.6 is the version to declare going forward). Anything older than 1.5 is a
    // hard error (pre-1.5 semantics are gone); anything newer than 1.6 is not known to this build.
    // An inline script without a version declaration defaults to 1.5 (mirrors
    // `parseVersion Nothing = NGLVersion 1 5`); a file always has a header (required when parsing).
    let version = match &script.header {
        Some(header) => {
            let declared = parse_version(&header.version).ok_or_else(|| {
                NgError::script(format!(
                    "Could not parse ngless version '{}'.",
                    header.version
                ))
            })?;
            if declared < MIN_VERSION {
                return Err(NgError::script(format!(
                    "Script declares ngless version \"{}\", but this build supports only ngless \"1.5\" and newer.\n\
                     Update the version statement, or use the Haskell build for older scripts.",
                    header.version
                )));
            }
            if declared > CURRENT_VERSION {
                return Err(NgError::script(format!(
                    "Script declares ngless version \"{}\", but this build implements only up to ngless \"1.6\".\n\
                     Update ngless, or lower the version statement.",
                    header.version
                )));
            }
            if declared == MIN_VERSION {
                output::warn(
                    0,
                    "This script declares ngless version \"1.5\". Going forward, ngless is based on the \
                     Rust implementation and the current version is \"1.6\", which is a drop-in \
                     replacement for \"1.5\". Please update the version statement to \"1.6\".",
                );
            }
            // Normalise to the effective semantics version (1.6 mirrors 1.5 exactly), mirroring the
            // Haskell `parseVersion (Just "1.6") = NGLVersion 1 5`.
            EFFECTIVE_VERSION
        }
        None => EFFECTIVE_VERSION,
    };
    // The module imports (empty for an inline script with no header).
    let modules: Vec<crate::ast::ModInfo> = script
        .header
        .as_ref()
        .map(|h| h.modules.clone())
        .unwrap_or_default();
    let temp_dir = std::path::PathBuf::from(&config.temporary_directory);

    // Gather the functions contributed by imported modules. Built-in standard modules
    // (`samtools`/`mocat`/...) are handled by `module_functions`; everything else is loaded as an
    // external YAML module (`Modules/<name>.ngm/<version>/module.yaml`).
    let mut extra_funcs = Vec::new();
    let mut external_modules = Vec::new();
    let mut constants: Vec<(String, NGLType)> = Vec::new();
    let mut constant_values = Vec::new();
    for m in &modules {
        // `motus` and `soap` are legacy standard modules that are no longer supported. Importing
        // either aborts with a guidance error rather than being loaded (mock modules). `motus`
        // mirrors StandardModules/Motus.hs, which — at any version ≥1.4, i.e. every supported
        // script — throws for the deprecated "1.0" wrapper and points newer versions at the
        // downloadable `motus.ngm`. `soap` (StandardModules/Soap.hs) registered the long-obsolete
        // SOAP mapper, which was never ported; we reject it up front instead.
        match m.name() {
            "motus" => {
                return Err(NgError::script(if m.version() == "1.0" {
                    "motus module is not supported in NGLess 1.4 (it supported motus1 only and \
                     that is now very old; please see \
                     https://github.com/ngless-toolkit/ngless-contrib/tree/master/motus.ngm)"
                        .to_string()
                } else {
                    "To use the motus module for newer versions, you need to download it and \
                     import it with 'local import'"
                        .to_string()
                }));
            }
            "soap" => {
                return Err(NgError::script(
                    "soap module is no longer supported (the SOAP mapper is very old and \
                     unmaintained; please use the 'bwa' or 'minimap2' mappers instead)"
                        .to_string(),
                ));
            }
            _ => {}
        }
        constants.extend(crate::modules::module_constants(m.name(), m.version()));
        constant_values.extend(crate::interpret::module_constant_values(
            m.name(),
            m.version(),
        ));
        // The `batch` module overrides the worker thread count from the batch scheduler's CPU
        // allotment at load time, superseding `--jobs` (mirrors the `setNumCapabilities` call in
        // StandardModules/Batch.hs, which runs while the module is loaded — after the command-line
        // thread count has been applied).
        if m.name() == "batch" {
            if let Some(ncpus) = crate::batch::get_ncpus() {
                crate::parallel::override_n_threads(ncpus);
            }
        }
        match crate::modules::module_functions(m.name(), m.version()) {
            Some(fs) => {
                // Internal/standard modules now track the ngless version: "1.6" is the canonical
                // version. Older versions still load (with the latest behaviour) but are deprecated.
                if m.version() != crate::modules::CURRENT_MODULE_VERSION {
                    output::warn(
                        0,
                        &format!(
                            "The built-in module \"{}\" is imported at version \"{}\". Going forward, \
                             internal modules track the ngless version; please update the import to \
                             version \"{}\".",
                            m.name(),
                            m.version(),
                            crate::modules::CURRENT_MODULE_VERSION,
                        ),
                    );
                }
                extra_funcs.extend(fs)
            }
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

    // IO validation (mirrors `validateIO`): eagerly check that input files exist, output
    // directories are writable, `map` references are known, and `count()` features are valid, so
    // these problems abort before any work is done. Runs before the citation header is printed,
    // matching Haskell's ordering.
    crate::validation::validate_io(&funcs, &config.search_path, &typed)?;

    if opts.validate_only {
        if !opts.quiet {
            eprintln!("Script OK.");
        }
        return Ok(0);
    }

    // The run header is suppressed by `--no-header`/the config `print-header` key, or when the
    // script writes to STDOUT (mirrors `nConfPrintHeader` and `setQuiet` on `uses_STDOUT`).
    if config.print_header && !crate::validation::uses_stdout(&typed) {
        let citations = crate::citations::collect_citations(&typed);
        crate::citations::print_header(&citations);
    }

    // Capture the validated, pre-transform script: the export modes serialise it as the "original"
    // script (mirrors `writeScriptJSON jsoname sc transformed`/`writeCWL sc ...`, where `sc` is the
    // validated — not transformed — script).
    let original_script = if opts.export_json.is_some() || opts.export_cwl.is_some() {
        Some(typed.clone())
    } else {
        None
    };

    // Post-validation transforms (mirrors `Transform.transform`, run after `validate`). Inject
    // the `__hash` keyword argument into `write`/`collect` calls so `auto_comments=[{hash}]` can
    // report a content hash that is byte-identical to the Haskell build.
    let mut typed = typed;
    crate::transform::add_output_hash(
        &mut typed.body,
        (version.major, version.minor),
        &modules,
        &funcs,
    );
    // The parallel module contributes its own transform (`run_for_all`/`set_parallel_tag`/lock
    // hash). It runs after `add_output_hash`, mirroring Haskell's pre-transforms-then-module order.
    if let Some(parallel) = modules.iter().find(|m| m.name() == "parallel") {
        let include_for_all = parallel.version() == "1.1"
            || parallel.version() == crate::modules::CURRENT_MODULE_VERSION;
        crate::transform::parallel_transform(&mut typed.body, include_for_all)
            .map_err(NgError::script)?;
    }
    // The samtools module's `sortOFormat` transform: when a `samtools_sort` result is only used in
    // a BAM `write`, inject `__output_bam=True` so the interpreter sorts straight to BAM (one pass,
    // one `@PG` line). A module transform, so it runs after `addOutputHash` and before `writeToMove`.
    if modules.iter().any(|m| m.name() == "samtools") {
        crate::transform::sort_oformat(&mut typed.body);
    }
    // `writeToMove`: mark `write()` calls whose input variable is dead afterwards with
    // `__can_move=True`, so the interpreter may move (rename) the backing temp file to the output
    // instead of copying it. First of the builtin transforms, before `addFileChecks`.
    crate::transform::write_to_move(&mut typed.body);
    // `ifLenDiscardSpecial`: rewrite `if len(read) <op> N: discard` inside preprocess blocks to the
    // `Optimized(LenThresholdDiscard ...)` fast path. A builtin transform, run after `writeToMove`
    // and before `addFileChecks`, matching Haskell's order. Output-neutral.
    crate::transform::if_len_discard_special(&mut typed.body);
    // Insert the floated `__check_ofile` calls (mirrors `addFileChecks`, a builtin transform that
    // runs after the module transforms). Done after output hashing so the inserted checks do not
    // affect the `{hash}`/`{script}` content hashes.
    typed.body = crate::transform::add_file_checks(std::mem::take(&mut typed.body), &funcs);
    // Insert the floated `__check_index_access` calls (mirrors `addIndexChecks`, a builtin transform
    // that runs after `addFileChecks`). A constant out-of-bounds index now fails early, right after
    // the array is assigned. Done after output hashing so the inserted checks do not affect hashes.
    typed.body = crate::transform::add_index_checks(std::mem::take(&mut typed.body));

    // Export modes (gated on `--experimental-features`, checked above): serialise the script and
    // exit before interpretation, mirroring the `whenJust (exportJSON ...) $ ... exitSuccess` /
    // `whenJust (exportCWL ...) $ ... exitSuccess` block in `modeExec DefaultMode`. JSON gets both
    // the original (validated) and transformed scripts; CWL uses only the original.
    if let Some(original) = &original_script {
        if let Some(jsoname) = &opts.export_json {
            crate::export::write_script_json(jsoname, original, &typed)?;
            return Ok(0);
        }
        if let Some(cwlname) = &opts.export_cwl {
            crate::export::write_cwl(original, &fname, cwlname)?;
            return Ok(0);
        }
    }

    // ARGV: for a file script, `[script_path, ...extra_args]`; for an inline script, just the
    // extra args (mirrors `nConfArgv`: `ScriptFilePath f -> f:extraArgs`, otherwise `extraArgs`).
    let mut argv = Vec::new();
    if opts.inline_script.is_none() {
        argv.push(fname.clone());
    }
    argv.extend(opts.extra_args.iter().cloned());

    // Active mappers (mirrors `ngleMappersActive`): bwa is always available; importing the
    // `minimap2` module activates that mapper. (`soap` would activate the SOAP mapper in Haskell,
    // but importing `soap` now aborts up front — see the module loop above.)
    let mut active_mappers = vec!["bwa".to_string()];
    for m in &modules {
        if m.name() == "minimap2" {
            active_mappers.push("minimap2".to_string());
        }
    }

    crate::interpret::interpret(
        &typed.body,
        &temp_dir,
        config.keep_temporary_files,
        &text,
        &config.search_path,
        &argv,
        opts.subsample,
        external_modules,
        constant_values,
        active_mappers,
        (version.major, version.minor),
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
