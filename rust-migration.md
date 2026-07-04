# NGLess Rust: status

The Rust implementation is the sole, supported NGLess. **Byte-for-byte parity with the removed
Haskell binary is no longer a goal** — the build now supports a single language version (`ngless
"1.6"`) and has dropped the multi-version / effective-version machinery, the deprecated `count`
`strand` argument, the `--search-dir` / `--check-deprecation` flags, and the Haskell-`Show`
reproduction that the output content hash used to depend on (the `{hash}` value is now an internal
identifier and differs from earlier releases).

## Known behavioral gaps

- **`Transform.hs` passes:** `addTemporaries` is not ported -- the only
  observable gap is `<call>()[<constInt>]` (indexing a call result directly,
  without binding it to a variable).
