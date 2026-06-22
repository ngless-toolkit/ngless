# NGLess — Rust reimplementation (work in progress)

This directory holds the in-progress Rust rewrite of NGLess. See `../rust-migration.md`
for the full scoping and execution plan.

**Status:** Milestone 1 — CLI scaffold only. The interpreter is not implemented yet.

## Build & test

```bash
cd rust
cargo build --release      # produces target/release/ngless
cargo test                 # unit tests
```

## Parity check against the functional test suite

The functional tests under `../tests/` are the parity oracle. Point the existing harness
at the Rust binary via `NGLESS_BIN`:

```bash
# from the repository root
NGLESS_BIN=rust/target/release/ngless ./run-tests.sh
```

The committed `expected.*` files in each test directory are the Haskell binary's output, so
passing `run-tests.sh` with the Rust binary *is* parity with Haskell. (Most tests fail today
because the interpreter is a stub — that is expected at milestone 1.)
