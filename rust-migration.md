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
>   `__assert`/`to_string`. The citation header and `clap`-based arg parsing are still
>   pending, so functional-test stdout parity is not yet attempted.
> - **M4 (FASTQ path, partial):** the pure trimming core (`substrim`/`endstrim`/`smoothtrim`,
>   encode/decode, `compatibleHeader`) plus a **file-backed** single-end `fastq` →
>   `preprocess(...) using |read|:` → `write` path. Read sets reference FASTQ files on disk
>   (mirroring `FastQFilePath`/`ReadSet`): `fastq` keeps the original file with its detected
>   encoding, `preprocess` streams it through the block (read slicing, `len`,
>   `discard`/`continue`, read-write block variable, and the `avg_quality`/`fraction_at_least`/
>   `n_to_zero_quality` methods) to a fresh temp file, and `write` copies the current file.
>   This file-backed model is what makes `write` byte-identical to its input.
>   Compressed FASTQ I/O is handled transparently for **gzip** (a small `compression` module
>   over `flate2`, dispatched on file extension): `fastq` reads `.fq.gz`, and `write`
>   recompresses on format change or copies bytes verbatim when formats match (mirroring
>   `moveOrCopyCompress`). bzip2/zstd report a clear "not supported yet" error.
>   Paired-end read sets are supported: `paired(m1, second=m2, singles=m3)` references the mate
>   files (encoding-checked, empty singles dropped), `preprocess` processes mates in lockstep
>   (both survive → pair, one survives → singleton via `keep_singles`), and `write` derives
>   `pair.1`/`pair.2`/`singles` names (`_formatFQOname`) and concatenates per-slot files.
>   FASTQ QC statistics are collected as `fastq`/`paired`/`preprocess` run (per-file base
>   composition, GC/non-ATCG fractions, sequence-length range, encoding), and `qcstats({fastq})`
>   serialises them to the transposed TSV (mirroring `writeOutputTSV`); `write` of the resulting
>   counts file copies it out. This needed a faithful port of Haskell's `show :: Double -> String`
>   (fixed vs. scientific notation, e.g. `3.896103896103896e-2`) in `values::show_double`.
>   **First functional tests now pass against the Rust binary** (identical output to the
>   committed `expected.*`): `tests/write_fq`, `tests/write_fq_inline`, `tests/preprocess`
>   (copy, substrim, endstrim, smoothtrim, `avg_quality` filter, `n_to_zero_quality`),
>   `tests/regression-fqgz` (gz input), `tests/preprocess3_empty_singles` (paired preprocess),
>   and `tests/preprocess3` (paired preprocess + gz output + `qcstats` TSV). Their
>   `ngless "1.1"` headers were bumped to `"1.5"` (these features have only minimum-version
>   checks, no version-conditional behavior, so Haskell output is unchanged).
>   Simplifications to lift next: files are read whole rather than streamed (no
>   `FileOrStream`/bounded queues), bzip2/zstd compression, and per-position quality
>   percentiles.
> - **M5 (mapping + SAM, partial):** a SAM data layer (`src/sam.rs`: full 12-field parse
>   faithful to the Haskell `SimpleParser` — including its quirk that an 11-column line leaves
>   `qual` empty and stores the qualities in `extra` — plus `encodeSamLine`, the flag predicates,
>   CIGAR `matchSize`/`matchIdentity`, `samIntTag` and `fixCigar`). On top of it:
>   `samfile` (reference a SAM file, or merge a `headers=` file in front of it) as a file-backed
>   `MappedReadSet`; `as_reads` (reconstruct FASTQ from SAM records — grouped by read name, mates
>   split by flag, lone reads as singletons; mirrors `samToFastQ`); and `select` in both forms —
>   the call form (`keep_if`/`drop_if`, mirrors `executeSelect`) and the block form
>   (`select(mapped) using |mr|:` with `mr.filter(min_match_size/min_identity_pc/max_trim/
>   action/reverse)` and `mr.flag({mapped})`, mirrors `executeSelectWBlock`), including the
>   "sequence reinjection" + `fixCigar` path (`src/select.rs`). `write` of a `MappedReadSet`
>   copies/recompresses the backing SAM. Passing: `tests/as_reads_encoding`,
>   `tests/sam_reverse_order`, `tests/select_sam_optional_fields`, `tests/fix_cigarSam`,
>   `tests/samfile-headers` (the `ngless "1.1"` headers bumped to `"1.5"`; `select`/`filter`
>   have no behavior change at ≥1.1, since `addUseNewer` only injects the old-behaviour flag
>   *below* 1.1, and the select-0.8 reinject change is already on at ≥0.8).
> - **M5 (BAM + samtools module):** `src/samtools.rs` shells out to the external `samtools`
>   binary (`$NGLESS_SAMTOOLS_BIN` or `samtools` on PATH; the Haskell build bundles a pinned
>   one) for BAM↔SAM conversion (`convertSamToBam`/`convertBamToSam`), reading BAM as SAM text
>   (`samBamConduit`), `samtools sort` and `samtools view -L`. BAM input now flows through
>   `samfile`/`select`/`as_reads`; `write` of a `MappedReadSet` to `.bam` (or `.sam.gz`)
>   converts/recompresses as needed. The `import "samtools" version "X"` statement is honoured —
>   `module_functions` contributes `samtools_sort` (0.0+) and `samtools_view` (0.1/1.0) to the
>   type checker, validation and interpreter. Passing: `tests/regression-cigar-filter`
>   (`samtools_sort by={name}` → write SAM) and `tests/regression-bad-sam` (`select` → write
>   SAM **and** BAM). `tests/samfile-write` works for its BAM-input case but is blocked by its
>   `.sam.zst` input (zstd decompression still pending). Still pending: `map` itself (needs
>   bwa/minimap2 — not installable here), zstd/bzip2 (de)compression, and the remaining
>   mapped-read methods (`pe_filter`/`unique`/`allbest`/`some_match`).

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
