# NGLess Rust: known discrepancies vs. Haskell parity

The following are the remaining behavioral differences between the Rust implementation and the
Haskell parity target for `ngless "1.5"`+ scripts.

- **HTML/JS run report.** Haskell's `Output.hs::writeOutputJS` writes a report directory
  (`output.js` + HTML) at end of run; `src/output.rs` has no report writer (only the console output
  layer). The per-position quality percentiles the report displays *are* now computed by the QC
  accumulator (`fastq.rs`, see below), so the data is ready once the report writer lands.

- **`Transform.hs` passes:** `addTemporaries` is not ported — the only
  observable gap is `<call>()[<constInt>]` (indexing a call result directly,
  without binding it to a variable).
