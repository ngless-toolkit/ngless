# Rewriting NGLess in Rust — Scoping & Execution Plan

> **Status:** Milestones 1–3 landed on branch `rust-migration`. The Rust crate lives at the
> repository root (`Cargo.toml`, `src/`), since it is intended to eventually replace the
> Haskell implementation in place. The functional test harness (`run-tests.sh`) can be
> pointed at any binary via the `NGLESS_BIN` environment variable.
>
> - **M1 (scaffold):** CLI info flags + `--check-install`, byte-matching the Haskell CLI.
> - **M2 (front end):** tokenizer, parser, AST, type checker, pure validation — all with
>   ported unit tests (`Tests/{Parse,Types,Validation}.hs`).
> - **M3 (core runtime, partial):** CLI flow (load → parse → version-gate `>=1.5` → type
>   check → validate → interpret), runtime values + `evalBinary/Unary/Index`, and an
>   interpreter for the pure subset plus `print`/`println`/`read_int`/`read_double`/
>   `__assert`/`to_string`. Not yet: `FileOrStream`, temp-file lifecycle, module loading,
>   and the data subsystems (so `fastq`/`map`/`count`/`write`/blocks return a clear
>   "not implemented yet"). The citation header and `clap`-based arg parsing are also still
>   pending, so functional-test stdout parity is not yet attempted.

## Context

NGLess is a ~15–16k-line Haskell program (`NGLess/`, 86 `.hs` files) implementing a
version-pinned DSL for NGS/metagenomics workflows, plus ~1.6k lines of unit tests and
112 functional tests under `tests/`. The motivation for a Rust rewrite is **not**
performance — Haskell/conduit already streams large files fine. The drivers are:

- **Build/maintenance pain** — GHC + Stack + Nix + `haskell.nix` materialized deps is a
  heavy, slow, hard-to-onboard toolchain; the Haskell contributor pool is small.
- **Contributors/ecosystem** — Rust has a larger contributor base and a strong
  bioinformatics crate ecosystem (`noodles`, `needletail`, `rust-bio`).
- **Distribution** — simpler single static binary and image builds (today this works but
  goes through Nix/musl/embedded-C machinery).

Decision taken: **full big-bang rewrite** aiming at behavioral parity, then cut over.

**Scope decision: only `ngless "1.5"` and newer are supported.** Scripts declaring older
versions (`0.5`–`1.4`) do not need to run on the Rust binary — they can be rejected with a
clear "version no longer supported; use the Haskell build or update your script" error.
This removes the entire version-aware semantics long tail (the single largest risk), and
lets us delete/skip the version-specific functional tests rather than reproduce historical
quirks. The cost is a hard break for old scripts in the wild — acceptable given the 1.5
cutover is itself a major version.

The non-negotiable success criterion is **reproducibility at version ≥ 1.5**: the new
binary must produce byte-identical (or semantically identical, where tool versions differ)
output to the Haskell one for current scripts. The existing `tests/` suite is the contract
that proves this.

## The single biggest asset: the functional test suite as a parity oracle

`tests/` (112 `.ngl` scripts + `expected.*` outputs, driven by `run-tests.sh`) is
**language-agnostic** — it runs a binary and diffs outputs. The entire rewrite should be
driven test-first against this suite:

1. Keep `tests/` and `run-tests.sh` unchanged.
2. Point `run-tests.sh` at the Rust binary via an env var (`NGLESS_BIN`).
3. Drop/skip the version-specific tests for `0.5`–`1.4` (e.g. `count-basic_0_5`); keep only
   `ngless "1.5"`+ cases as the parity contract.
4. "Done" for any milestone = the corresponding subset of `tests/` passes against the
   Rust binary with identical output to the Haskell binary.

This converts a risky semantics-preservation problem into a measurable, incremental one.
Do this plumbing first, before writing any interpreter code.

## What carries over cheaply vs. what is genuinely hard

**Cheap / mechanical:**
- AST and type enums (`Language.hs`, 286 lines) → Rust `enum`s with `serde` derives.
- Type checker (`Types.hs`, 407 lines) → straightforward single-pass inference; accumulate
  errors in a `Vec` instead of the `Writer` monad.
- Validation passes (`Validation.hs`, 360 lines) → list of pure functions over the AST.
- Error model (`NGError.hs`) → `enum NgErrorType` + `thiserror`/`anyhow`.
- Module YAML loading (`ExternalModules.hs`) → `serde_yaml` structs.
- External tool invocation — bwa/samtools/minimap2/megahit/prodigal are **already
  subprocesses**; this is a `std::process`/`tokio::process` wrapper, not a port. Keep the
  pinned versions in `Dependencies/Versions.hs` as a Rust constants module.

**Genuinely hard / risk-bearing:**
- **Conduit streaming model** (`Interpret.hs`, `Utils/Conduit.hs`, `Data/FastQ.hs`,
  `Interpretation/FastQ.hs`) — bounded async queues (`TBMQueue`), batched vectors, async
  gzip. Must be re-expressed with Rust channels/iterators without blowing up memory or
  changing output ordering. This is the architectural core to get right.
- **Feature counting** (`Interpretation/Count.hs`, 988 lines — the largest subsystem) —
  annotation modes, overlap resolution, multi-mapper handling, normalization (raw / fpkm /
  scaled). Subtle and heavily tested; port last and lean hard on `tests/count*`.
- **FileOrStream abstraction** (`FileOrStream.hs`) — the file-vs-lazy-stream duality threads
  through everything; needs a clean Rust equivalent (an enum yielding a boxed iterator)
  designed up front.
- **Temp-file lifecycle / GC** — Haskell's `ResourceT` + laziness drive cleanup of
  intermediate files. Rust needs explicit ownership (RAII guard types) for temp files and
  the script-hash-based output caching in `Transform.hs`.

## Crate mapping (recommended)

| Haskell | Rust |
|---|---|
| parsec / `Tokens.hs`+`Parse.hs` | `chumsky` or `winnow` (hand-written recursive descent is also fine — grammar is small) |
| conduit / conduit-algorithms | std `Iterator` + `crossbeam-channel` for bounded queues; `rayon` for data parallelism |
| Data.FastQ (+inline C) | `needletail` or `noodles-fastq` (drop the C FFI) |
| Data.Sam | `noodles-sam` / `noodles-bam` |
| Data.GFF / Data.Fasta | `noodles-gff` / `noodles-fasta` |
| zlib / bzlib-conduit | `niffler` (transparent gz/bz2/zstd) |
| aeson / yaml | `serde_json` / `serde_yaml` |
| http-conduit, tar | `reqwest`/`ureq` + `tar`/`flate2` |
| async / stm / unliftio | `std::thread` + `crossbeam`, or `tokio` if going async |
| optparse-applicative | `clap` (derive) |
| edit-distance (suggestions) | `strsim` |
| tasty / HUnit / QuickCheck | `cargo test` + `proptest`; functional tests stay as-is |
| inline-c-cpp RefSeqInfoVector | pure-Rust interned `Vec`/`FxHashMap` (no FFI) |

Note this **eliminates all bundled C/C++/FFI** (`FastQ.c`, `RefSeqInfoVector.h`,
`embedded.c`) — a real maintenance win aligned with the stated motivation.

## Execution milestones (each gated by a `tests/` subset passing vs. Haskell)

1. **Harness + scaffold.** Cargo workspace; `run-tests.sh` driving `NGLESS_BIN`; CI that
   runs both binaries and diffs. Stub binary that parses argv. **← current**
2. **Front end.** Tokenizer → parser → AST → type checker → validation. Gate: all
   `tests/error-validation-*` and parse/type unit tests reproduce identical diagnostics.
3. **Core runtime + FileOrStream + temp-file lifecycle + config/CLI**
   (`Configuration.hs`, `CmdArgs.hs`, `NGLEnvironment.hs`, `Output.hs`). Gate: trivial
   scripts (print, write, argv) pass.
4. **FASTQ path.** `fastq`/`paired`/`group`, preprocess blocks, substrim/endstrim/
   smoothtrim, unique, QC stats, the streaming pipeline. Gate: `tests/` preprocess/fastq/
   write_fq cases.
5. **Mapping + SAM.** `map` (bwa/minimap2/soap wrappers), `samfile`, `select`, `as_reads`,
   samtools module. Gate: map*/select/samfile* tests.
6. **Counting.** `count`, `countfile`, `mapstats`, all modes + normalizations. Gate:
   count* tests (the hardest parity surface).
7. **Modules + stdlib.** External YAML modules, parallel/lock-file framework
   (`StandardModules/Parallel.hs`, `Utils/LockFile.hs`), assemble/orf_find, mocat/motus,
   reference DB download + index cache (`ReferenceDatabases.hs`). Gate: assemble/parallel/
   module tests.
8. **Version gate.** Parse the `ngless "X.Y"` header; accept `≥ 1.5`, reject older versions
   with a clear, actionable error. No historical behavior switches to reproduce (out of
   scope per the scope decision above).
9. **Distribution cutover.** Cargo-based static musl binary, Docker image, **rewrite the
   bioconda recipe** (Haskell→Rust build), macOS build. Run full `tests/` on all targets.
10. **Docs + release.** `docs/sources` content is largely language-level and stays; verify
    every example script runs. Ship as a major version; keep the Haskell binary published
    in parallel for one release cycle as a fallback.

## Critical files to mirror (highest leverage)

- `NGLess/Language.hs`, `Types.hs`, `Parse.hs`, `Tokens.hs`, `Validation.hs`,
  `Transform.hs` — the front end (ignore the `< 1.5` version branches within them).
- `NGLess/Interpret.hs`, `Utils/Conduit.hs`, `FileOrStream.hs` — runtime + streaming core.
- `NGLess/Interpretation/Count.hs` — largest and most parity-sensitive.
- `NGLess/Data/{FastQ,Sam,GFF,Fasta}.hs` — replaced by noodles/needletail.
- `NGLess/ExternalModules.hs`, `Modules.hs`, `StandardModules/Parallel.hs` — module system.
- `NGLess/Dependencies/Versions.hs` — pinned external tool versions (copy verbatim).
- `run-tests.sh`, `tests/` — the acceptance oracle (do not change semantics).

## Risks & honest assessment

- **Effort:** with pre-1.5 support dropped, realistically **6–12 months** of focused work
  for a small team, now dominated by milestone 6 (counting) and the streaming core, not by
  line count or version back-compat. The front end is weeks.
- **Parity over performance:** since speed isn't the goal, resist "improving" behavior
  mid-port — any semantic change that fails `tests/` is a regression, not a feature. Bank
  improvements for *after* parity is reached and the Haskell binary is retired.
- **Streaming correctness** is the top technical risk: bounded-memory behavior on
  multi-GB inputs and stable output ordering must be validated on large real inputs, which
  `tests/` (small fixtures) won't catch. Add a few large-input soak tests.
- **Reproducibility across tool versions:** outputs depend on bwa/samtools versions
  (recent commits already updated expected outputs for newer samtools). Pin identical tool
  versions during the parity phase so diffs reflect *NGLess* behavior, not tool drift.
- **Two-codebase interval:** for the duration, bug fixes must land in both or be frozen in
  Haskell. Declare a feature freeze on the Haskell side at milestone 4.

## Verification

- Primary: `NGLESS_BIN=<rust-binary> ./run-tests.sh` — must reach 112/112 with diffs
  identical to the Haskell binary, run per-milestone on the gated subset.
- Differential CI job: build both binaries, run each `tests/*` script through both, fail on
  any output diff (the strongest possible parity check).
- `cargo test` for ported unit tests (parse/type/count/validation), plus `proptest` for the
  tokenizer/parser round-trips.
- Large-input soak tests (multi-GB FASTQ/BAM) to confirm bounded memory and ordering.
- Final: run every example script in `docs/sources` end-to-end; build + smoke-test the
  bioconda, Docker, static-musl, and macOS artifacts.
