# NGLess Rust: known discrepancies vs. Haskell parity

The following are the remaining behavioral differences between the Rust implementation and the
Haskell parity target for `ngless "1.5"`+ scripts.

## Current parity gaps

- **`Transform.hs` passes:** `addTemporaries` is not ported -- the only
  observable gap is `<call>()[<constInt>]` (indexing a call result directly,
  without binding it to a variable).
- **`preprocess(..., keep_singles=False)` is ignored.** Rust hardcodes
  `keep_singles = true`; Haskell reads the `keep_singles` keyword with default `True`.
  Scripts that rely on orphaned paired-end mates being discarded will keep extra singleton
  reads in Rust.
- **`write(mapped, format={sam|bam})` ignores explicit `format`.** Rust infers SAM/BAM
  solely from `ofile`; Haskell lets `format` override filename inference. A script writing
  `format={sam}` to a non-`.sam*` filename, or `format={bam}` to a non-`.bam` filename,
  can produce a different format.
- **`.xz` compression is unsupported.** Rust handles uncompressed, `.gz`, `.bz2`, and
  `.zst`/`.zstd`. Haskell also recognized `.xz` for compressed I/O.
- **External modules are less complete.** Haskell external modules can return
  `sequenceset` values and perform filetype-specific SAM/BAM/compression conversion for
  module arguments. Rust currently handles non-void external command returns for `counts`
  and `mappedreadset`; other return types error, and argument file conversion is more
  limited.
