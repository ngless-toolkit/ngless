# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

NGLess is a domain-specific language for NGS (next-generation sequencing) data processing,
aimed at metagenomics pipelines (FASTQ preprocessing → mapping → feature counting). The
`.ngl` script language is versioned; scripts declare a version on their first line (e.g.
`ngless "1.5"`).

**Implementation status (important):** NGLess was rewritten from Haskell to Rust. As of the
1.6 release the Haskell implementation was *completely removed* and the Rust code at the repo
root (`Cargo.toml`, `src/`) is the sole, supported implementation. Many doc comments in `src/`
are **stale** — they still describe features as "deferred to a later milestone" or "a scaffold"
even where the Rust code fully implements them. Do not trust those; treat the Rust code as the
full implementation. `rust-migration.md` is the most
accurate status doc (all 99 functional tests pass against the Rust binary). Current crate
version is `1.6.0-beta1` (see `Cargo.toml`).

## Build & test

```sh
cargo build --release          # produces target/release/ngless
cargo test                     # unit tests (in-module #[cfg(test)])
cargo test <name>              # run a single unit test by name
cargo fmt --all -- --check     # formatting is enforced in CI
```

Functional / parity test suite (the primary correctness bar):

```sh
NGLESS_BIN=target/release/ngless ./run-tests.sh          # all 99 tests
NGLESS_BIN=target/release/ngless ./run-tests.sh regression   # only tests/regression*
```

`run-tests.sh` iterates `tests/*/`. Each test dir contains one or more `*.ngl` scripts and
committed `expected.*` files; the harness runs ngless and `diff`s actual output against
`expected.*`. A dir may also have `cmdargs` (extra CLI args), `run.sh` (custom invocation),
`check.sh` (extra assertions), and `cleanup.sh`. Dirs named `error-*` expect a non-zero exit;
`error-validation-*` run with `-n` (validate only). The `expected.*` files were produced by the
old Haskell binary, so passing the suite *is* a byte-for-byte parity check against Haskell.

**External tools** (samtools, bwa, minimap2, prodigal, megahit) are NOT bundled. The binary
finds them on `$PATH` or via per-tool overrides `NGLESS_SAMTOOLS_BIN`, `NGLESS_BWA_BIN`,
`NGLESS_MINIMAP2_BIN`, `NGLESS_PRODIGAL_BIN`, `NGLESS_MEGAHIT_BIN`. `pixi.toml` pins these
tools; CI and local runs get them via pixi:

```sh
pixi run --environment default bash -c 'NGLESS_BIN="$PWD/target/release/ngless" ./run-tests.sh'
```

CI is `.github/workflows/build_rust.yml`.

## ChangeLog

Update `ChangeLog` for any user-visible change (new/changed/removed functions, flags,
output, bug fixes) and for large internal changes. Add a bullet under the current unreleased
version block at the top, matching the existing tab-indented `* ...` style.

## Architecture

The execution pipeline (mirrors the Haskell `DefaultMode` flow) is, in order:

**load → tokenize → parse → version gate (≥1.5) → type check → validate → transform → interpret**

Entry: `src/main.rs` → `lib.rs::run` (handles `--version*`, `--help`, `--check-install`,
`--print-path`, `--debug-parse`) → `cli.rs::run_cli` / `run_script` drives the pipeline above.

Front end:
- `tokens.rs` — tokenizer.
- `ast.rs` — AST types (`Expression`, `FuncName`, `NGLType`, `Block`, …).
- `parser.rs` — hand-written recursive-descent parser over the token stream (ports Parsec
  `NGLess/Parse.hs`; same AST, error *messages* not yet byte-identical).
- `types.rs` — type inference/checking; annotates each `Lookup` with its type.
- `validation.rs` — pure validation passes (semantic checks beyond typing).
- `modules.rs` — builtin function/method signatures and argument checks (`ArgCheck`), plus
  `NGLVersion`. `external_modules.rs` loads user `.ngm` external modules (see `Modules/`).

Transform & run:
- `transform.rs` — post-typecheck AST transforms. Notably `addOutputHash`, which computes an
  MD5 content hash injected as a hidden `__hash` arg (reported by `auto_comments=[{hash}]`).
  This hash must be **byte-identical** to Haskell, so `show_expr` reproduces Haskell's derived
  `Show`.
- `interpret.rs` — the interpreter (largest module). Evaluates the pure subset plus the
  file-backed FASTQ/SAM pipeline builtins (`fastq`, `paired`, `preprocess ... using |read|:`,
  `map`, `select`, `count`, `write`, `qcstats`, `collect`, sample loading, …).
- `values.rs` — runtime values (`NGLessObject`) and operators; includes `show_double`, a
  faithful port of Haskell's `show :: Double -> String` needed for byte-identical numeric output.

Domain subsystems (called from the interpreter):
- `fastq.rs` / `compression.rs` — file-backed read sets, streaming FASTQ records, QC stats;
  transparent gzip/bzip2/zstd/plain I/O via `open_read` and `StreamWriter`.
- `sam.rs`, `mapper.rs`, `minimap2.rs`, `samtools.rs` — mapping and SAM/BAM handling.
- `count.rs`, `gff.rs`, `select.rs` — feature counting, GFF parsing, SAM record filtering.
- `reference.rs` — reference-database resolution / auto-download (tar + HTTP via `ureq`).
- `parallel.rs` — in-process compute parallelism: the `--jobs`/`--threads` thread config and
  `par_map_ordered` (order-preserving bounded parallel map). Note the `.ngl` `parallel` module
  itself (`lock1`, `collect`) is implemented in `interpret.rs`/`modules.rs`/`transform.rs`.
- `batch.rs` — the `batch` standard module: reads scheduler env vars (`LSB_JOBINDEX`,
  `SGE_TASK_ID`, `SLURM_CPUS_PER_TASK`, …) to expose job-array constants and override the thread count.

Cross-cutting:
- `configuration.rs` — config-file + env + CLI settings (mirrors `Configuration.hs`).
- `lockfile.rs`, `tempfiles.rs`, `cleanup.rs` — lock files, temp-file management, and
  signal-driven cleanup (removes locks/temps on Ctrl+C / SIGTERM).
- `output.rs`, `citations.rs`, `errors.rs`, `suggestion.rs` — run header/verbosity, citation
  collection, error types, and "did you mean" suggestions.
- `progress.rs` — single-line terminal progress bar with ETA (port of `Utils/ProgressBar.hs`);
  drawn via `output::transient_msg` for the download and bwa-mapping paths.
- `export.rs` — `--export-json` (mirrors `JSONScript.hs`).

## Byte-parity is the core constraint

The overriding design goal is *behavioral parity* with the removed Haskell implementation for
`ngless "1.5"`+ scripts. Much of the code exists to reproduce Haskell output exactly (numeric
formatting, output hashes, version/header strings, citation ordering, error exit codes). When
changing anything that affects output, run the functional suite — a `diff` against `expected.*`
is the regression gate. Version 1.6 deliberately maps to 1.5 semantics (see `EFFECTIVE_VERSION`
in `cli.rs`) so `{hash}` and other output stay identical across 1.5/1.6 scripts.
