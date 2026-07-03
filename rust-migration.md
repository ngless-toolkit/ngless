# NGLess Rust: known discrepancies vs. Haskell parity

The following are the remaining behavioral differences between the Rust implementation and the
Haskell parity target for `ngless "1.5"`+ scripts.

## Current parity gaps

- **`Transform.hs` passes:** `addTemporaries` is not ported -- the only
  observable gap is `<call>()[<constInt>]` (indexing a call result directly,
  without binding it to a variable).
- **`.xz` compression is unsupported.** Rust handles uncompressed, `.gz`, `.bz2`, and
  `.zst`/`.zstd`. Haskell also recognized `.xz` for compressed I/O.
- **External modules are less complete.** Haskell external modules can return
  `sequenceset` values and perform filetype-specific SAM/BAM/compression conversion for
  module arguments. Rust currently handles non-void external command returns for `counts`
  and `mappedreadset`; other return types error, and argument file conversion is more
  limited.
