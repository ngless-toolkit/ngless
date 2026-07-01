//! A single-line terminal progress bar with ETA, a faithful port of the (unreleased) 1.5+
//! `NGLess/Utils/ProgressBar.hs`.
//!
//! Like the Haskell version, the bar draws only on a terminal and throttles redraws to at most once
//! per second, and only when the displayed per-mille of progress changes. The rendered line matches
//! the Haskell format:
//!
//! ```text
//! <name>: [====    ]   50.0% (12s elapsed; ETA: 00:12)
//! ```
//!
//! Drawing goes through [`crate::output::transient_msg`], the self-clearing status-line mechanism
//! (mirroring Haskell's `writeTransientMsg`): the line is rewritten in place and is erased whenever
//! a durable log line is printed. The two callers are the download path
//! ([`crate::reference::download_file`]) and bwa mapping ([`crate::mapper::call_mapper`], via
//! [`ProgressWriter`]), matching `Utils/Network.hs` and `StandardModules/Mappers/Bwa.hs`.

use std::io::{IsTerminal, Write};
use std::time::Instant;

pub struct ProgressBar {
    /// Text prefix printed before the bar (e.g. `"Mapping FASTQ files"`).
    name: String,
    /// Width of the bar (number of character cells between the brackets).
    width: usize,
    /// Per-mille (`round(progress * 1000)`) last drawn, so we skip redraws that would not change
    /// the visible line. Starts at `-1000` to mirror Haskell's initial `cur = -1`.
    last_permille: i64,
    /// When the line was last redrawn; redraws are throttled to at most once per second.
    last_updated: Instant,
    /// When the bar was created; the basis for the "elapsed" time.
    started: Instant,
    /// A `(progress, time)` snapshot taken ~10s in, used to estimate speed for the ETA while
    /// ignoring the initial burst of activity (mirrors `snapshot0`). Starts at `(0, started)`.
    snapshot: (f64, Instant),
    /// Whether drawing is active. False when the output stream is not a terminal, making every
    /// method a no-op (mirrors `mkProgressBar` returning `Nothing`).
    enabled: bool,
}

impl ProgressBar {
    /// Create a progress bar `width` cells wide (mirrors `mkProgressBar`). If the output stream
    /// (stderr, where NGLess sends its diagnostics) is not a terminal the bar is inert and never
    /// draws. Haskell checks `stdout` in `mkProgressBar` but writes via the configured output
    /// handle; we consistently use the stream we actually draw on.
    pub fn new(name: impl Into<String>, width: usize) -> ProgressBar {
        let now = Instant::now();
        ProgressBar {
            name: name.into(),
            width,
            last_permille: -1000,
            last_updated: now,
            started: now,
            snapshot: (0.0, now),
            enabled: std::io::stderr().is_terminal(),
        }
    }

    /// Redraw the bar for fractional `progress` in `[0, 1]` (mirrors `updateProgressBar`). Does
    /// nothing if the bar is inert, if less than a second has passed since the last redraw, or if
    /// the per-mille of progress is unchanged.
    pub fn update(&mut self, progress: f64) {
        if !self.enabled {
            return;
        }
        let now = Instant::now();
        let permille = (progress * 1000.0).round() as i64;
        if now.duration_since(self.last_updated).as_secs_f64() <= 1.0
            || permille == self.last_permille
        {
            return;
        }

        // Estimate ETA (mirrors the commented derivation in ProgressBar.hs):
        //   1. ignore the first 10s (initial activity is often bursty),
        //   2. then snapshot (time, progress),
        //   3. ignore the next 10s unless >=10% is done,
        //   4. estimate speed from progress since the snapshot,
        //   5. pad by 5% (users prefer an over-estimate).
        let elapsed = now.duration_since(self.started).as_secs_f64();
        let (snap_progress, snap_time) = self.snapshot;
        let elapsed_delayed = now.duration_since(snap_time).as_secs_f64();
        let eta = if elapsed < 10.0 {
            "no ETA yet".to_string()
        } else if snap_time == self.started {
            // First redraw past the 10s warm-up: take the reference snapshot now.
            self.snapshot = (progress, now);
            "no ETA yet".to_string()
        } else if elapsed < 20.0 && progress < 0.1 {
            "no ETA yet".to_string()
        } else {
            let denom = progress - snap_progress;
            if denom > 0.0 {
                let missing = (1.0 - progress) * elapsed_delayed / denom;
                format!("ETA: {}", show_secs(1.05 * missing))
            } else {
                "no ETA yet".to_string()
            }
        };

        let line = format!(
            "{}: {} {} ({} elapsed; {})",
            self.name,
            draw_bar(self.width, progress),
            percentage(progress),
            show_secs(elapsed),
            eta
        );
        crate::output::transient_msg(&line);
        self.last_permille = permille;
        self.last_updated = now;
    }
}

/// A [`Write`] adapter that drives a [`ProgressBar`] from the number of newlines passing through it.
///
/// This mirrors Haskell's `progressFQ`, which counts `'\n'` bytes (`B.count 10`) in the FASTQ
/// stream fed to `bwa mem` and reports `newlines / (4 * total_reads)` (four lines per FASTQ record).
/// Wrapping the writer means both the line-by-line paired writes and the raw singleton copy in
/// `write_interleaved` are accounted for without special-casing either.
pub struct ProgressWriter<W: Write> {
    inner: W,
    bar: ProgressBar,
    /// Denominator: total newlines expected (`4 * total_reads`).
    total_newlines: u64,
    seen_newlines: u64,
}

impl<W: Write> ProgressWriter<W> {
    /// Wrap `inner`, reporting progress against `total_reads` FASTQ records (each 4 lines).
    pub fn new(
        inner: W,
        name: impl Into<String>,
        width: usize,
        total_reads: u64,
    ) -> ProgressWriter<W> {
        ProgressWriter {
            inner,
            bar: ProgressBar::new(name, width),
            total_newlines: total_reads.saturating_mul(4),
            seen_newlines: 0,
        }
    }
}

impl<W: Write> Write for ProgressWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let n = self.inner.write(buf)?;
        if self.total_newlines > 0 {
            self.seen_newlines += buf[..n].iter().filter(|&&b| b == b'\n').count() as u64;
            self.bar
                .update(self.seen_newlines as f64 / self.total_newlines as f64);
        }
        Ok(n)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

/// Render `[====    ]`, `bars` equals signs then padding spaces (mirrors `drawProgressBar`).
fn draw_bar(width: usize, progress: f64) -> String {
    let bars = (progress * width as f64).round().max(0.0) as usize;
    let spaces = width.saturating_sub(bars);
    format!("[{}{}]", "=".repeat(bars), " ".repeat(spaces))
}

/// Format the percentage as `%6.1f%%` does in Haskell, e.g. `"  12.3%"` (mirrors `showPercentage`).
fn percentage(progress: f64) -> String {
    format!("{:6.1}%", progress * 100.0)
}

/// Format a duration in seconds as Haskell's `showSecs` does: `"45s"`, `"12:03"` (mm:ss),
/// `"01:02:03"` (hh:mm:ss), or `"2-03:04:05"` (days-hh:mm:ss).
fn show_secs(t: f64) -> String {
    let secs = t.round() as i64;
    let secsr = secs % 60;
    let mins = secs / 60;
    let minsr = mins % 60;
    let hours = mins / 60;
    let hoursr = hours % 24;
    let days = hours / 24;
    if secs < 60 {
        format!("{secs}s")
    } else if secs < 60 * 60 {
        format!("{mins:02}:{secsr:02}")
    } else if days == 0 {
        format!("{hours:02}:{minsr:02}:{secsr:02}")
    } else {
        format!("{days}-{hoursr:02}:{minsr:02}:{secsr:02}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bar_rendering() {
        assert_eq!(draw_bar(10, 0.0), "[          ]");
        assert_eq!(draw_bar(10, 1.0), "[==========]");
        assert_eq!(draw_bar(10, 0.5), "[=====     ]");
        // Overshoot clamps the spaces at zero rather than underflowing.
        assert_eq!(draw_bar(4, 1.5), "[======]");
    }

    #[test]
    fn percentage_formatting() {
        assert_eq!(percentage(0.0), "   0.0%");
        assert_eq!(percentage(0.123), "  12.3%");
        assert_eq!(percentage(1.0), " 100.0%");
    }

    #[test]
    fn show_secs_formats() {
        assert_eq!(show_secs(0.0), "0s");
        assert_eq!(show_secs(45.0), "45s");
        assert_eq!(show_secs(59.4), "59s");
        assert_eq!(show_secs(60.0), "01:00");
        assert_eq!(show_secs(723.0), "12:03"); // 12m03s
        assert_eq!(show_secs(3723.0), "01:02:03"); // 1h02m03s
        assert_eq!(show_secs(180_245.0), "2-02:04:05"); // 2d02h04m05s
    }
}
