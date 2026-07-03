# NGLess Rust: known discrepancies vs. Haskell parity

The following are the remaining behavioral differences between the Rust implementation and the
Haskell parity target for `ngless "1.5"`+ scripts.

- **`Transform.hs` passes:** `addTemporaries` is not ported — the only
  observable gap is `<call>()[<constInt>]` (indexing a call result directly,
  without binding it to a variable).
