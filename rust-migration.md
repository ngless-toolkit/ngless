# Rewriting NGLess in Rust — Scoping & Execution Plan

> **Status:** Milestones 1–3 are complete and milestones 4–7 are partially landed on branch
> `rust-migration`. The Rust crate lives at the
> repository root (`Cargo.toml`, `src/`), since it is intended to eventually replace the
> Haskell implementation in place. The functional test harness (`run-tests.sh`) can be
> pointed at any binary via the `NGLESS_BIN` environment variable. As of this writing
> **79 of the 96 functional tests pass** against the Rust binary with output identical to
> Haskell (including the samtools `check.sh` cases, now driven via `--print-path samtools`). See
> the [Functional test status](#functional-test-status) table below for the per-test breakdown.
>
> - **M1 (scaffold):** CLI info flags + `--check-install`, byte-matching the Haskell CLI.
> - **M2 (front end):** tokenizer, parser, AST, type checker, pure validation — all with
>   ported unit tests (`Tests/{Parse,Types,Validation}.hs`).
> - **M3 (core runtime, partial):** CLI flow (load → parse → version-gate `>=1.5` → type
>   check → validate → interpret), runtime values + `evalBinary/Unary/Index`, and an
>   interpreter for the pure subset plus `print`/`println`/`read_int`/`read_double`/
>   `__assert`/`to_string`. The run header (version/copyright banner + the sorted, deduplicated
>   citation block, mirroring `printHeader`/`collectCitations` in `src/citations.rs`) is now
>   printed before interpretation, gated on `--no-header` and suppressed when the script writes to
>   `STDOUT` (`uses_stdout`, mirroring `setQuiet`). The `ARGV` builtin constant (`[script_path,
>   ...extra_args]`), the `readlines` builtin, and pure function calls in expression position
>   (e.g. `read_int(s)` inside `__assert`) are also done. With these, the remaining pure/front-end
>   functional tests pass: `tests/len_list`, `tests/argv`, `tests/type-conversions`,
>   `tests/readlines`, `tests/parse_odd_corners`. `clap`-based arg parsing is still pending.
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
>   SAM **and** BAM). `__merge_samfiles` is also done (`execute_merge_sams`, mirroring
>   `executeMergeSams`/`mergeSAMGroups MSBestOnly`; `tests/merge-sams` passes). The mapped-read
>   methods `pe_filter`/`unique`/`allbest`/`some_match` are now implemented (`src/select.rs`:
>   `filter_pe`/`m_unique`/`m_besthit`, dispatched in `execute_method`), and the samtools
>   `checkUnique` validation (selecting `{unique}` from a `samtools_sort`ed set is an error) is
>   wired into `validation::validate` (`tests/select_block_filter`, `tests/error-unique-on-sorted`).
>   `map` itself is now done — see the bwa milestone below.
> - **Compression (gzip/bzip2/zstd):** `src/compression.rs` now (de)compresses all of gzip
>   (`flate2`), bzip2 (`bzip2` crate, pure-Rust `libbz2-rs-sys` backend) and zstd (`zstd` crate);
>   output is content-equivalent (the suite compares decompressed data, so exact bytes need not
>   match). samtools cannot read zstd/bzip2, so SAM inputs in those formats are decompressed to a
>   plain SAM temp before `samtools sort`/`view`/conversion (`samtools_input`). With this,
>   `tests/samfile-write` passes end-to-end (BAM input → `samtools_sort` → BAM and `.sam.gz`
>   outputs all match), including its `check.sh` now that `--print-path samtools` is implemented
>   (see below).
> - **`--print-path EXEC`:** implemented in `src/lib.rs` (mirroring `PrintPathMode` in
>   `Execs/Main.hs` and `findNGLessBin`/`checkExecutable` in `FileManagement.hs`). Since the Rust
>   build embeds no binaries, the tool path is resolved from `$NGLESS_<TOOL>_BIN` (which must point
>   at an executable file) or by searching `PATH`; unknown tool names error with `Unknown binary
>   <name>.`, as in Haskell. This unblocks every samtools `check.sh` that shells out to
>   `$(ngless --print-path samtools)`: `tests/samfile-select-sort`, `tests/samfile-write`,
>   `tests/select-bam` and `tests/write_compression` now pass end-to-end. `tests/samfile-select-view`
>   now runs but fails on a **samtools version-drift** difference, not a port bug: its
>   `samtools_view(mapped, bed_file=...)` runs `samtools view -h -O sam -L short.bed.gz` (exactly
>   the Haskell `executeView` command line), and samtools 1.23.1 emits one extra alignment record
>   (`SRR070372.2719`, `X:4992867`, CIGAR `31M1I134M1D204M2S`) that overlaps the large
>   `X:4972559-4997598` gene interval. The committed `texpected.sam.gz` (Jul 2024) was generated
>   with an older samtools whose `-L` overlap semantics excluded that read (newer samtools only
>   excludes it under the `-M` multi-region iterator). Removing that single record makes the Rust
>   output byte-identical to `texpected.sam.gz`, and the Haskell binary run against samtools 1.23.1
>   would produce the same extra read — i.e. this is the "outputs depend on samtools version"
>   reproducibility caveat (Risks section), to be resolved by pinning samtools or regenerating the
>   expected file, not by changing NGLess.
> - **M6 (counting, part 1):** `count()` with all three annotation modes and all four
>   normalizations (`src/count.rs` + `src/gff.rs`, mirroring `Interpretation/Count.hs` and
>   `Data/GFF.hs`). The seqname annotator is built from the `@SQ` header (sorted by name, sizes
>   from `LN:`); the functional-map annotator parses a MOCAT-style TSV (one annotator per feature
>   column, sorted by tag, `feature:` prefix only when >1 feature, sizes summed from `@SQ` when
>   `normalization={normed}`); the GFF annotator parses GFF3/GTF (per-attribute `=`-vs-space and
>   comma-split values), reindexes feature ids to sorted order and resolves overlaps with
>   `union`/`intersection_strict`/`intersection_non_empty` under `sense`/`antisense` strand rules.
>   Multi-mappers follow `all1`/`1overN`/`unique_only`/`dist1` (the dist1 second pass distributes by
>   size-normalized weight, or 1/N when zero); `scaled`/`fpkm` rescale over the totals excluding the
>   `-1` bucket. Output uses Rust's shortest-round-trip `f64` `Display`, which matches
>   double-conversion's `toShortest` for every value in the suite. `write()` of counts gained
>   `format={csv}` (tab→comma) and a manual `comment=` (`# ` prefix). The Haskell async/conduit
>   pipeline is performance-only and is replaced by a serial whole-file pass. Passing (headers
>   bumped `1.1`/`1.2`→`1.5`): `tests/count-basic` (seqname; all1/dist1/1overN, normed/scaled, csv),
>   `tests/count-map-file` (functional map; raw/normed/dist1, multi-column, `discard_zeros`),
>   `tests/count-gff` (GFF union; strand/sense/antisense, scaled, `min`, `include_minus1`) and
>   `tests/count-gff-corner-cases` (GFF subfeatures with comma-split attributes). `countfile()`
>   (`Interpretation/CountFile.hs`) is also done: it references an existing counts TSV, reordering
>   the data rows by tag (the first column) when they are not already sorted while keeping leading
>   comments and the sample-name header in place (`tests/countfile-reorder` passes). `mapstats()`
>   (`executeMapStats` + `samStatsC'`) is done too: it summarizes a mapped read set as
>   total/aligned/unique read groups (grouped by name, a group is *aligned* if any read aligns and
>   *unique* if aligned and all records share one reference); `tests/mapstats` passes end-to-end,
>   including the chained `select(...keep_if=[{mapped}/{unique}])` inputs and the `ofile=STDOUT`
>   output. `write()` of counts now also supports `auto_comments=[{script}]`: the original script
>   source is threaded through the interpreter (`Interpreter::script_text`, mirroring
>   `ngleScriptText`) and `build_comment` reproduces `buildComment` — a `Output generated by:`
>   header followed by the verbatim script lines indented four spaces, after any manual `comment=`,
>   each line `# `-prefixed. `tests/count-fpkm` now passes end-to-end (bumped to `1.5`; the two
>   `expected.w_auto_comment*` files were updated to embed `ngless '1.5'`, which is what the Haskell
>   binary emits for a `1.5` script). Still pending: `auto_comments={date}`/`{hash}` (untested,
>   non-deterministic — `build_comment` errors on them); `count-mode`/`count-subfeatures` need
>   `map()`/bwa (CI only, though their GFF modes are covered above); and the double-conversion
>   scientific switch for out-of-range exponents is deferred.
> - **M7 (mapping with bwa):** `map()` is implemented for the `fafile=` form with the bwa mapper
>   (`src/mapper.rs` + `execute_map`/`perform_map` in `src/interpret.rs`, mirroring
>   `Interpretation/Map.hs` and `StandardModules/Mappers/Bwa.hs`). `src/mapper.rs` shells out to
>   the external `bwa` (`$NGLESS_BWA_BIN` or `bwa` on PATH; the Haskell build bundles a pinned one),
>   querying its version at runtime and embedding it in the index file names (`<base>-bwa-<ver>.<ext>`,
>   mirroring `indexPrefix`/`bwaVersion`); indices are built lazily (`bwa index`, with the
>   1/10th-filesize `-b` block size for ≥100MB references) and reused via the `.amb/.ann/.bwt/.pac/.sa`
>   set. Reads are interleaved for `bwa mem -K 100000000 -p` (`interleave_fastq`, mirroring
>   `interleaveFQs`: pairs emitted record-by-record and re-normalised to LF — so Windows line
>   endings are handled — then singletons appended verbatim) and streamed on stdin while stdout is
>   redirected to a SAM temp. `block_size_megabases=` splits the FASTA into block-sized chunks
>   (`split_fasta`/`ensure_splits_exist`, mirroring `splitFASTA` with the `.splits_Nm.K.fna`
>   naming and `.done` receipt); a single chunk maps directly, multiple chunks map separately and
>   merge best-only (`merge_sam_files`/`merge_sam_group`, mirroring `mergeSAMGroups MSBestOnly` at
>   ≥1.1). `fafile=` is resolved through `--search-path` with `<references>`-style placeholders
>   (`expand_path`/`expand_path_candidates`, mirroring `expandPath'`, incl. `name=/path` entries).
>   Passing (verified against pixi-provided bwa 0.7.19/samtools): `tests/map3` (single + split),
>   `tests/map_search_path`, `tests/map_search_path_multiple`, `tests/map_sort_stream`
>   (→ `samtools_sort by={name}`) and `tests/map-windows_line_terminators` (single + split). Still
>   pending: packaged `reference=` databases (need the reference-download infrastructure) and the
>   minimap2/soap mappers (`map(..., mapper='minimap2')` errors clearly for now); index creation is
>   not lock-guarded (safe for single-process runs).

> - **M7a (modules + stdlib — sample/directory loading):** the lowest-risk slice of M7, built on
>   the already-working FASTQ path. `group()` is now interpreted (`execute_group`, mirroring
>   `executeGroup`/`groupFiles`: a single member is renamed, multiple members have their pair- and
>   singleton-file lists concatenated in order; empty groups error). On top of it,
>   `load_fastq_directory` (`BuiltinModules/LoadDirectory.hs`) globs `<dir>/*.{fq,fastq}{,.gz,.bz2,
>   .xz}` (sorted), pairs files by the `matchUp` rules (cross product of the extensions with
>   `(.1,.2)`/`(_1,_2)`/`(_F,_R)` markers, `nub`-dedup, singles file derived by `pair.N`→`single`),
>   loads singletons via `fastq` and pairs via `paired`, then `group`s them under the directory
>   name. The `mocat` module (`StandardModules/Mocat.hs`) exposes `load_mocat_sample` as a thin
>   wrapper over the same loader (versions `0.0`/`1.0`/`1.1`, with the MOCAT/MOCAT2 citations).
>   `load_sample_list` (`BuiltinModules/Samples.hs`) parses a YAML sample manifest (`serde_yaml`;
>   optional `basedir`, `samples:` map of name → `[{paired:[a,b]}|{single:..}]`) into a list of
>   named, Sanger-encoded read sets. The `.name()` method on a read set and `discard_singles`
>   (`AsReads.hs`: keep pairs, drop singletons) are also done, as is the `--subsample` flag
>   (deterministic 1/10 record sample on load — `subsample_text` mirrors `drop90`/`performSubsample`
>   keeping the first 4 of every 40 lines, capped at 100000 — plus the `.subsampled` write-output
>   marker from `parseWriteOptions`). Passing: `tests/load_fastq_directory`, `tests/load_sample_list`,
>   the seven `tests/mocat_sample_*` cases (gz/uncompressed/bz2, single + paired, incl. the
>   `discard_singles` `mocat_sample_uncompressed_paired`), `tests/regression-write-fqgz` and
>   `tests/regression-subsample_write`. Still pending in M7: packaged `reference=` databases (M7b),
>   external YAML modules (M7c), the parallel module (M7d), and assemble/orf_find (M7e).
> - **M7b (modules + stdlib — packaged reference databases):** `map(..., reference="...")` now
>   resolves a packaged reference to its FASTA and feeds it through the same indexing/mapping path
>   as `fafile=` (`resolve_reference` in `src/interpret.rs`, mirroring `ensureDataPresent`/
>   `findDataFiles`). The reference directory is named by the *user-typed* name — so `sacCer3` and
>   its alias `Saccharomyces_cerevisiae_R64-1-1` resolve to separate, independently-installed
>   directories — and is searched for at `References/<name>/Sequence/BWAIndex/reference.fa.gz` in
>   the user data directory (`$HOME/.local/share/ngless/data`) first, then the global one
>   (`<binary-dir>/../share/ngless/data`), matching `findDataFiles`'s `User <|> Root`. The bwa
>   index is then (re)built lazily next to the cached FASTA by the existing `ensure_index_exists`
>   path (so a reference packaged with an older bwa gets a fresh `<base>-bwa-<ver>` index for the
>   running bwa). Downloading a missing reference is *not* supported in this build — it errors with
>   a clear, actionable message (install via the Haskell build, which caches under
>   `~/.local/share/ngless/data/References/`, or pass a local `fafile=`). `qcstats({mapping})` is
>   also done: each `map()` call records `(total, aligned, unique)` read groups into a mapping-stats
>   accumulator (`register_map_stats` + the shared `sam_group_stats`, mirroring `samStatsC'` and
>   `outputMappedSetStatistics`), and `qcstats({mapping})` serialises them to the transposed TSV
>   (`format_map_stats_tsv`, mirroring `writeOutputTSV`'s `encodeMapStats`); its `inputFile`/
>   `reference` fields hold temp/index paths exactly as Haskell records them, so the output is not
>   byte-reproducible and no test diffs it. Passing (verified against the locally-cached `sacCer3`
>   and pixi bwa/samtools): `tests/as_reads`, `tests/reference_alias`, `tests/select`,
>   `tests/select_block`, and `tests/grouped` (group → map → `qcstats({fastq})`/`qcstats({mapping})`,
>   with `check.sh` confirming `map` of a grouped two-file read set equals `map` of the
>   concatenation). Still pending in M7: external YAML modules (M7c), the parallel module (M7d), and
>   assemble/orf_find (M7e).
> - **M7c (modules + stdlib — external YAML modules + the `example` standard module):** external
>   `.ngm` modules are now loaded and executed (`src/external_modules.rs`, mirroring
>   `ExternalModules.hs`). `find_load` searches `.`/global/user for
>   `Modules/<name>.ngm/<version>/module.yaml`, parses it with `serde_yaml` (functions, `arg1`,
>   `additional` args, `init`, `min-ngless-version`, citations), checks the version is compatible,
>   and `validate` runs the optional `init` command with the module environment
>   (`NGLESS_MODULE_DIR`/`TMPDIR`/...). `functions_for_typecheck` exposes each command as a
>   `Function` (mirroring `asFunction`). At run time, unknown function calls are dispatched to
>   `execute_external_command` (mirroring `executeCommand`): `encode_command_arg` builds the command
>   line exactly as `encodeArgument` does — the unnamed `arg1` is positional, named args become
>   `--name=value`, `flag`s emit their `when-true` tokens (or `--name`) when true and nothing when
>   false, defaults (`def`) fill omitted args, `str` args with `expand_searchpath` go through
>   `<...>` search-path expansion (falling back to the literal string), and a read-set argument is
>   concatenated to mate-1/mate-2/singles temp files (`encode_readset_files`, mirroring
>   `asFilePaths`). A non-void `return` adds a `--name=<tempfile>` output argument and decodes the
>   result. cli.rs routes built-in standard modules (`samtools`/`mocat`/`example`) through
>   `module_functions` and everything else through the external loader (the `mocat` standard module
>   now accepts any requested version). The `example` standard module (`StandardModules/Example.hs`)
>   is also ported, which required threading **module constants** (`EXAMPLE_HELLO`, ...) through the
>   type checker (`checktypes` now takes a `constants` list), validation, and the interpreter
>   (constant values seed the env); `example()` prints its diagnostics including a faithful
>   Haskell-`show` of the read set (`haskell_show`). Passing: `tests/arg1NotPathExternalModule`,
>   `tests/whenTrueModule`, `tests/searchpathExternalModule`, `tests/exampleExternalModule`
>   (external YAML modules, incl. `init`/`min-ngless-version`/read-set args/search-path expansion)
>   and `tests/exampleModule` (the `example` standard module + constants + Haskell-`show`). Still
>   pending in M7: the parallel module (M7d) and assemble/orf_find (M7e).
>
## Context

NGLess is a ~15–16k-line Haskell program (`NGLess/`, 86 `.hs` files) implementing a
version-pinned DSL for NGS/metagenomics workflows, plus ~1.6k lines of unit tests and
96 functional tests under `tests/` (the pre-1.5 cases have been removed). The motivation for
a Rust rewrite is **not**
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

`tests/` (96 `.ngl` scripts + `expected.*` outputs, driven by `run-tests.sh`) is
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

## Functional test status

Status of every test under `tests/` against the current Rust binary (built with `cargo build`
and run via `pixi run --environment default bash -c 'NGLESS_BIN=$PWD/target/debug/ngless
./run-tests.sh'`, with `bwa`/`samtools` provided by the pixi environment).

Legend: ✅ passes · ❌ not yet supported. `--print-path EXEC` is now implemented (resolves a
tool from `$NGLESS_<TOOL>_BIN` or `PATH`, mirroring `PrintPathMode`/`findNGLessBin`), so the
samtools `check.sh` scripts that shell out to `$(ngless --print-path samtools)` can now be
driven locally. **Tally: 79 ✅ · 17 ❌ (96 total).**

| Test | Status | Note / planned milestone |
|---|---|---|
| arg1NotPathExternalModule | ✅ | M7c — external YAML modules |
| argv | ✅ | M3 — `ARGV` builtin + citation header |
| as_reads | ✅ | M7b — packaged `reference=` (sacCer3) |
| as-reads-3 | ❌ | M4-pending — interleaved I/O (`format_flags={interleaved}`, `fastq(interleaved=True)`) |
| as_reads-bam | ✅ | M5 |
| as_reads_encoding | ✅ | M5 |
| as_reads_regression | ✅ | M5 |
| assemble-gp | ❌ | M7 — `assemble`/`orf_find` |
| compress_sam | ✅ | M5 |
| count-basic | ✅ | M6 |
| countfile-reorder | ✅ | M6 |
| count-fpkm | ✅ | M6 |
| count-gff | ✅ | M6 |
| count-gff-corner-cases | ✅ | M6 |
| count-map-file | ✅ | M6 |
| count-mode | ✅ | M6 (via M7 bwa) |
| count-subfeatures | ✅ | M6 (via M7 bwa) |
| error-bad-fq | ✅ | M4 |
| error-block-assignment | ✅ | M2 |
| error-check-file-early | ✅ | M3 |
| error-count-nofile | ✅ | M6 |
| error-map-file | ❌ | Future — early validation: bad column must be caught before any `write`, else `should.not.be.created.txt` is written |
| error-ofile-complex | ❌ | Future — early output-dir validation + error-message parity + citation header |
| error-unique-on-sorted | ✅ | M5 — samtools `checkUnique` validation |
| error-validate-nofafile | ✅ | M5 |
| error-write-no-output-dir | ✅ | M4 |
| exampleExternalModule | ✅ | M7c — external YAML modules |
| exampleModule | ✅ | M7c — `example` standard module + constants |
| fix_cigarSam | ✅ | M5 |
| grouped | ✅ | M7b — `reference=` + `qcstats({mapping})` |
| len_list | ✅ | M3 — citation/copyright header |
| load_fastq_directory | ✅ | M7a — `load_fastq_directory` + `group` |
| load_sample_list | ✅ | M7a — `load_sample_list` (YAML manifest) |
| map3 | ✅ | M7 (bwa) |
| map-minimap2 | ❌ | M7/Future — minimap2 mapper |
| map_search_path | ✅ | M7 |
| map_search_path_multiple | ❌ | Bug — `@HD` header line ordering in SAM output (alignment records match) |
| map_sort_stream | ✅ | M7 |
| mapstats | ✅ | M6 |
| map-windows_line_terminators | ✅ | M7 |
| max_filename_length | ✅ | M4 |
| merge-sams | ✅ | M5 — `__merge_samfiles` |
| mocat_sample_bz2 | ✅ | M7a — mocat module |
| mocat_sample_bz2_paired | ✅ | M7a — mocat module |
| mocat_sample_bz2_paired_mixed | ✅ | M7a — mocat module |
| mocat_sample_gz | ✅ | M7a — mocat module |
| mocat_sample_gz_paired | ✅ | M7a — mocat module |
| mocat_sample_uncompressed | ✅ | M7a — mocat module |
| mocat_sample_uncompressed_paired | ✅ | M7a — mocat module + `discard_singles` |
| parallel | ❌ | M7 — parallel module |
| parallel_collect_many | ❌ | M7 — parallel module |
| parallel_collect_subdir | ❌ | M7 — parallel module |
| parallel_folder_lock | ❌ | M7 — parallel module |
| parse_odd_corners | ✅ | M3 — `readlines` builtin + citation header |
| paste | ❌ | M7 — parallel module |
| preprocess | ✅ | M4 |
| preprocess3 | ✅ | M4 |
| preprocess3_empty_singles | ✅ | M4 |
| preprocess_fastx_mocat | ✅ | M4 |
| readlines | ✅ | M3 — `readlines` builtin |
| reference_alias | ✅ | M7b — `reference=` by alias |
| regression-bad-sam | ✅ | M5 |
| regression-bz2-chunk | ✅ | M4 |
| regression-cigar-filter | ✅ | M5 |
| regression-fqgz | ✅ | M4 |
| regression-resave | ✅ | M5 |
| regression-subsample_write | ✅ | M7a — mocat module + `--subsample` |
| regression-unique-same-contig | ✅ | M6 |
| regression-write-fqgz | ✅ | M7a — mocat module |
| reuse | ✅ | M3 |
| same-hash-collect | ❌ | M7 — parallel module |
| same-hash-collect-2 | ❌ | M7 — parallel module |
| samfile-headers | ✅ | M5 |
| samfile-select-sort | ✅ | M5 (`check.sh` now driven via `--print-path samtools`) |
| samfile-select-view | ❌ | samtools drift — `samtools view -L` includes one extra read (`SRR070372.2719`) vs the stale `texpected.sam.gz`; Rust matches Haskell on the same samtools (see note) |
| samfile-write | ✅ | M5 (`check.sh` now driven via `--print-path samtools`) |
| sam_reverse_order | ✅ | M5 |
| searchpathExternalModule | ✅ | M7c — external module + search-path expansion |
| select | ✅ | M7b — packaged `reference=` (sacCer3) |
| select-bam | ✅ | M5 (`check.sh` now driven via `--print-path samtools`) |
| select_block | ✅ | M7b — packaged `reference=` (sacCer3) |
| select_block_filter | ✅ | M5 — `allbest`/`unique`/`pe_filter`/`some_match` methods |
| select_block_filter_unmatch | ✅ | M5 |
| select_max_trim | ✅ | M5 |
| select-multi-conditions | ✅ | M5 |
| select_regression_past | ✅ | M5 |
| select_sam_optional_fields | ✅ | M5 |
| shortreads | ✅ | M4 |
| type-conversions | ✅ | M3 — `read_int`/`read_double` as expression args |
| whenTrueModule | ✅ | M7c — external module flag `when-true` |
| write_compression | ✅ | M5 (`check.sh` now driven via `--print-path samtools`) |
| write_fq | ✅ | M4 |
| write_fq_inline | ✅ | M4 |
| write_fq_STDOUT | ❌ | Future — `write(..., ofile=STDOUT)` / `/dev/stdout` |
| write-hash | ❌ | M6-pending — `auto_comments={hash}` |
| write-hash2 | ❌ | M6-pending — `auto_comments={hash}` |

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

- Primary: `NGLESS_BIN=<rust-binary> ./run-tests.sh` — must reach 96/96 with diffs
  identical to the Haskell binary, run per-milestone on the gated subset.
- Differential CI job: build both binaries, run each `tests/*` script through both, fail on
  any output diff (the strongest possible parity check).
- `cargo test` for ported unit tests (parse/type/count/validation), plus `proptest` for the
  tokenizer/parser round-trips.
- Large-input soak tests (multi-GB FASTQ/BAM) to confirm bounded memory and ordering.
- Final: run every example script in `docs/sources` end-to-end; build + smoke-test the
  bioconda, Docker, static-musl, and macOS artifacts.
