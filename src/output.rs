//! Levelled diagnostic output, mirroring `NGLess/Output.hs`.
//!
//! NGLess classifies every status/diagnostic message by an [`OutputType`] level and prints it
//! only when the active [`Verbosity`] (and the `--trace` flag) call for it. This is the subsystem
//! that `--trace` modulates: with tracing on, *every* message — including the otherwise-hidden
//! [`OutputType::Trace`] level — is printed, with second-resolution timestamps.
//!
//! Unlike the Haskell version this layer does not maintain a self-clearing transient status line
//! (the single overwriting progress line shown on a terminal); messages are always printed as
//! durable lines. It also formats timestamps in UTC rather than the local zone. Neither matters
//! for the goal here, which is parity of *behaviour*, not byte-identical output.
//!
//! Configuration is process-global (set once via [`init`]), mirroring Haskell's global
//! `nglConfiguration` `IORef`: the message helpers are called from many places that do not thread
//! an interpreter/config handle (e.g. [`crate::reference`]).

use std::io::{IsTerminal, Write};
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};

/// Message severity/category, mirroring Haskell's `OutputType`. The ordering is significant:
/// `Trace < Debug < Info < Result < Warning < Error`, matching the derived `Ord` instance that
/// [`should_print`] relies on.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum OutputType {
    Trace,
    Debug,
    Info,
    Result,
    Warning,
    Error,
}

/// How much to print, mirroring Haskell's `Verbosity` (`-v/--verbosity quiet|normal|full`).
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Verbosity {
    Quiet,
    #[default]
    Normal,
    Loud,
}

struct OutputConfig {
    verbosity: Verbosity,
    /// `--trace`: print all levels (including [`OutputType::Trace`]) with second-resolution
    /// timestamps (mirrors `nConfTrace`).
    trace: bool,
    /// Whether to colour output (stderr is a terminal and `NO_COLOR` is unset).
    color: bool,
}

static CONFIG: OnceLock<OutputConfig> = OnceLock::new();

/// Initialise the global output configuration from the parsed command line. Called once at
/// startup; later calls are ignored (mirrors the single global `nglConfiguration`). `--quiet`
/// forces [`Verbosity::Quiet`] regardless of `-v` (mirrors `if quiet then Quiet else verbosity`).
pub fn init(verbosity: Verbosity, quiet: bool, trace: bool) {
    let verbosity = if quiet { Verbosity::Quiet } else { verbosity };
    let color = std::io::stderr().is_terminal() && std::env::var_os("NO_COLOR").is_none();
    let _ = CONFIG.set(OutputConfig {
        verbosity,
        trace,
        color,
    });
}

fn config() -> &'static OutputConfig {
    // Default to the same baseline as Haskell's `guessConfiguration` (Normal, no trace) so that
    // callers reached before `init` (e.g. in tests) still behave sensibly.
    CONFIG.get_or_init(|| OutputConfig {
        verbosity: Verbosity::Normal,
        trace: false,
        color: std::io::stderr().is_terminal() && std::env::var_os("NO_COLOR").is_none(),
    })
}

/// Whether a message of level `ot` should be printed under the current configuration. Mirrors the
/// combined logic of Haskell's `shouldPrint` and the `traceSet || …` guard in `output`:
/// `Trace` is shown only with `--trace`; with `--trace` everything else is shown too; otherwise
/// the threshold depends on the verbosity.
fn should_print(ot: OutputType) -> bool {
    let cfg = config();
    if ot == OutputType::Trace {
        return cfg.trace;
    }
    if cfg.trace {
        return true;
    }
    match cfg.verbosity {
        Verbosity::Quiet => ot >= OutputType::Warning,
        Verbosity::Normal => ot >= OutputType::Info,
        Verbosity::Loud => true,
    }
}

/// ANSI foreground colour code for a level (mirrors `colorFor`).
fn color_code(ot: OutputType) -> &'static str {
    match ot {
        OutputType::Trace | OutputType::Debug => "\u{1b}[37m", // white
        OutputType::Info => "\u{1b}[34m",                      // blue
        OutputType::Result => "\u{1b}[30m",                    // black
        OutputType::Warning => "\u{1b}[33m",                   // yellow
        OutputType::Error => "\u{1b}[31m",                     // red
    }
}

/// Emit a message at level `ot`, tagged with line number `lno` (0 means "no line number", as in
/// Haskell where `lno > 0` gates the `Line N` suffix). Output goes to stderr. This is the single
/// choke point mirroring Haskell's `output`.
pub fn message(ot: OutputType, lno: usize, msg: &str) {
    if !should_print(ot) {
        return;
    }
    let cfg = config();
    let tstr = timestamp(cfg.trace);
    let line_str = if lno > 0 {
        format!(" Line {lno}")
    } else {
        String::new()
    };
    let mut out = std::io::stderr().lock();
    let _ = if cfg.color {
        let (col, reset) = (color_code(ot), "\u{1b}[0m");
        writeln!(out, "{col}[{tstr}{line_str}]: {msg}{reset}")
    } else {
        writeln!(out, "[{tstr}{line_str}]: {msg}")
    };
}

/// `Trace`-level message (shown only under `--trace`); the Rust analogue of `traceStatus`.
pub fn trace(lno: usize, msg: &str) {
    message(OutputType::Trace, lno, msg);
}

/// `Info`-level message (shown at `Normal` verbosity and above).
pub fn info(lno: usize, msg: &str) {
    message(OutputType::Info, lno, msg);
}

/// `Warning`-level message (shown unless `--quiet` on a terminal silences below warnings — here,
/// always shown at `Quiet` and above).
pub fn warn(lno: usize, msg: &str) {
    message(OutputType::Warning, lno, msg);
}

/// Current time as `Day DD-MM-YYYY HH:MM` (UTC), matching Haskell's `%a %d-%m-%Y %R` used for the
/// parallel-module `.finished` receipt line.
pub(crate) fn finished_timestamp() -> String {
    timestamp(false)
}

/// Format the current time as `Day DD-MM-YYYY HH:MM[:SS]` in UTC. With `with_seconds` (i.e. under
/// `--trace`) the seconds field is included, mirroring Haskell's `%a %d-%m-%Y %T` vs `%R`.
fn timestamp(with_seconds: bool) -> String {
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    let (y, mo, d, dow) = civil_from_days(secs.div_euclid(86_400));
    let tod = secs.rem_euclid(86_400);
    let (hh, mm, ss) = (tod / 3600, (tod % 3600) / 60, tod % 60);
    const DOW: [&str; 7] = ["Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed"];
    let day = DOW[(dow % 7) as usize];
    if with_seconds {
        format!("{day} {d:02}-{mo:02}-{y:04} {hh:02}:{mm:02}:{ss:02}")
    } else {
        format!("{day} {d:02}-{mo:02}-{y:04} {hh:02}:{mm:02}")
    }
}

/// Convert a count of days since the Unix epoch (1970-01-01, a Thursday) to a civil
/// `(year, month, day, weekday)` tuple, where weekday is days-since-epoch mod 7 (0 = Thursday).
/// Uses Howard Hinnant's `civil_from_days` algorithm.
fn civil_from_days(z: i64) -> (i64, i64, i64, i64) {
    let dow = z.rem_euclid(7);
    let z = z + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d, dow)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ordering_matches_haskell() {
        assert!(OutputType::Trace < OutputType::Debug);
        assert!(OutputType::Info < OutputType::Warning);
        assert!(OutputType::Warning < OutputType::Error);
    }

    #[test]
    fn civil_from_days_known_dates() {
        // 1970-01-01 was a Thursday (weekday code 0).
        assert_eq!(civil_from_days(0), (1970, 1, 1, 0));
        // 2000-01-01 was a Saturday: days from epoch = 10957.
        assert_eq!(civil_from_days(10_957), (2000, 1, 1, 2));
    }
}
