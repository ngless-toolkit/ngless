# Rewriting NGLess in Rust — Port Record

> **Status: DONE.** The Rust rewrite is complete and is now the sole implementation of NGLess;
> the Haskell program was removed at the 1.6 release. The Rust crate lives at the repository root
> (`Cargo.toml`, `src/`). **All 99 functional tests pass** against the Rust binary with output
> byte-identical to the (now-removed) Haskell binary — including the samtools `check.sh` cases,
> driven via `--print-path samtools` — and the unit tests (`cargo test`) pass. The functional
> harness (`run-tests.sh`) is pointed at any binary via the `NGLESS_BIN` environment variable;
> the committed `expected.*` files were produced by the Haskell binary and remain the parity
> contract.
>
> This document began as the scoping/execution plan and is kept as the port record: the sections
> below describe what was ported (accurate) and, further down (from "## Context" onward), preserve
> the original planning notes — decisions, crate recommendations, risk assessment — for historical
> reference. Where those planning notes describe intended future work or recommend a crate, they
> may not match the final implementation (e.g. the parser is hand-written and the FASTQ/SAM code
> does not use `noodles`).

## Implemented surface (by area)

- **CLI:** all sub-modes and flags (see the CLI sub-modes entry under "Known remaining gaps"),
  with the info flags + `--check-install` byte-matching the Haskell CLI.
- **Front end:** tokenizer, parser, AST, type checker, pure validation — all with ported unit
  tests (`Tests/{Parse,Types,Validation}.hs`).
- **Core runtime:** CLI flow (load → parse → version-gate `>=1.5` → type check → validate →
  interpret), runtime values + `evalBinary/Unary/Index`, and an interpreter for the pure subset plus
  `print`/`println`/`read_int`/`read_double`/`__assert`/`to_string`. The run header (version/copyright
  banner + the sorted, deduplicated citation block, mirroring `printHeader`/`collectCitations` in
  `src/citations.rs`) is printed before interpretation, gated on `--no-header` and suppressed when the
  script writes to `STDOUT` (`uses_stdout`, mirroring `setQuiet`). The `ARGV` builtin constant
  (`[script_path, ...extra_args]`), the `readlines` builtin, and pure function calls in expression
  position (e.g. `read_int(s)` inside `__assert`) are done.
- **FASTQ path:** the pure trimming core (`substrim`/`endstrim`/`smoothtrim`, encode/decode,
  `compatibleHeader`) plus a **file-backed** `fastq` → `preprocess(...) using |read|:` → `write` path.
  Read sets reference FASTQ files on disk (mirroring `FastQFilePath`/`ReadSet`): `fastq` keeps the
  original file with its detected encoding, `preprocess` streams it through the block (read slicing,
  `len`, `discard`/`continue`, read-write block variable, and the `avg_quality`/`fraction_at_least`/
  `n_to_zero_quality` methods) to a fresh temp file, and `write` copies the current file. This
  file-backed model is what makes `write` byte-identical to its input. Compressed FASTQ I/O is handled
  transparently for **gzip** (a small `compression` module over `flate2`, dispatched on file
  extension); `write` recompresses on format change or copies bytes verbatim when formats match
  (mirroring `moveOrCopyCompress`). Paired-end read sets are supported: `paired(m1, second=m2,
  singles=m3)` references the mate files (encoding-checked, empty singles dropped), `preprocess`
  processes mates in lockstep (both survive → pair, one survives → singleton via `keep_singles`), and
  `write` derives `pair.1`/`pair.2`/`singles` names (`_formatFQOname`) and concatenates per-slot files.
  FASTQ QC statistics are collected as `fastq`/`paired`/`preprocess` run (per-file base composition,
  GC/non-ATCG fractions, sequence-length range, encoding), and `qcstats({fastq})` serialises them to
  the transposed TSV (mirroring `writeOutputTSV`). This needed a faithful port of Haskell's `show ::
  Double -> String` (fixed vs. scientific notation, e.g. `3.896103896103896e-2`) in
  `values::show_double`.
- **FASTQ streaming:** the FASTQ path does not read whole files into
  memory. Two content-agnostic streaming primitives in `src/compression.rs`: `open_read(path) ->
  Box<dyn BufRead>` (decompressing by extension) and `StreamWriter` (an enum over the
  gzip/bzip2/zstd/plain encoders — each `finish(self)` consumes the concrete type — replicating
  `write_bytes`'s compression levels, with a mandatory `finish`). On top of them `src/fastq.rs` has
  `fastq_records` (a lazy `ShortRead` iterator with `fq_decode`'s verbatim error messages),
  `FastQStatsAcc` (an incremental `stats_from_reads` fold, sequence-only so it is encoding-independent),
  and `detect_encoding_stream` (prefix-only). Every FASTQ consumer in `src/interpret.rs` is bounded
  streaming: `load_and_qc` (`fastq`/`paired`) does two bounded passes (detect then a streaming stats
  fold); `uninterleave` streams through three `StreamWriter`s with a one-record holdover;
  `interleave_fastq` became `write_interleaved<W: Write>` (bwa streams it straight to `bwa mem` stdin;
  minimap2 keeps buffering, as it already slurps all of stdout to sort); `execute_preprocess` opens
  three writers + three `FastQStatsAcc`s, streams pairs in lockstep writing each surviving read
  immediately; and the multi-file `write` concat streams via `io::copy` through one `StreamWriter`. The
  model is **per-stage bounded memory**: intermediate results still go to file-backed temp files
  (`NGLessObject` carries no lazy `Stream`), matching Haskell's `asFile`-at-every-`write` reality;
  cross-stage lazy streaming and the threaded `TBMQueue`/`asyncMapC` pipeline are deliberately out of
  scope (performance-only). `fastq_records` is generic over `BufRead`, so the primitives serve SAM/count
  conversions too.
- **SAM streaming:** the SAM read path does not
  slurp whole files into memory for the main consumers. `src/sam.rs` has `open_sam(path) -> SamReader`
  (transparently decompresses gzip via `compression::open_read` and decodes BAM by piping `samtools
  view -h` through `samtools::bam_to_sam_child` rather than capturing all of stdout — the child is
  waited on and a non-zero exit reported at EOF) and `group_sam_stream(path, paired) -> SamGroups`, a
  lazy `Iterator<Item = NgResult<SamItem>>` reproducing the old whole-file `group_sam` grouping exactly
  (header lines verbatim; consecutive same-read-name records — and, when not `paired`, the same
  first-in-pair bit — form one data group). Consumers: `execute_select`/`execute_select_block` stream
  groups straight to the output SAM line-by-line (`sam_temp_writer`); `execute_count` peels the leading
  `@SQ` header items into `sq_header` then feeds the remaining groups lazily — `count::perform_count`
  takes `IntoIterator<Item = NgResult<Vec<SamLine>>>`; `execute_mapstats`/`register_map_stats` fold over
  groups via `sam_group_stats_stream`; and `merge_sam_files` reads each partial via the streaming
  iterator. `execute_as_reads` is the one remaining whole-file SAM consumer (its output is O(n)
  regardless).
- **Mapping + SAM data layer:** `src/sam.rs` does a full 12-field parse faithful to the Haskell
  `SimpleParser` — including its quirk that an 11-column line leaves `qual` empty and stores the
  qualities in `extra` — plus `encodeSamLine`, the flag predicates, CIGAR `matchSize`/`matchIdentity`,
  `samIntTag` and `fixCigar`. On top of it: `samfile` (reference a SAM file, or merge a `headers=` file
  in front of it) as a file-backed `MappedReadSet`; `as_reads` (reconstruct FASTQ from SAM records,
  mirrors `samToFastQ`); and `select` in both forms — the call form (`keep_if`/`drop_if`, mirrors
  `executeSelect`) and the block form (`select(mapped) using |mr|:` with
  `mr.filter(min_match_size/min_identity_pc/max_trim/action/reverse)` and `mr.flag({mapped})`, mirrors
  `executeSelectWBlock`), including the "sequence reinjection" + `fixCigar` path (`src/select.rs`). The
  mapped-read methods `pe_filter`/`unique`/`allbest`/`some_match` are implemented (`src/select.rs`:
  `filter_pe`/`m_unique`/`m_besthit`, dispatched in `execute_method`), and the samtools `checkUnique`
  validation (selecting `{unique}` from a `samtools_sort`ed set is an error) is wired into
  `validation::validate`. `__merge_samfiles` is done (`execute_merge_sams`, mirroring
  `executeMergeSams`/`mergeSAMGroups MSBestOnly`).
- **BAM + samtools module:** `src/samtools.rs` shells out to the external `samtools` binary
  (`$NGLESS_SAMTOOLS_BIN` or `samtools` on PATH) for BAM↔SAM conversion
  (`convertSamToBam`/`convertBamToSam`), reading BAM as SAM text (`samBamConduit`), `samtools sort` and
  `samtools view -L`. BAM input flows through `samfile`/`select`/`as_reads`; `write` of a
  `MappedReadSet` to `.bam` (or `.sam.gz`) converts/recompresses as needed. The `import "samtools"
  version "X"` statement is honoured — `module_functions` contributes `samtools_sort` (0.0+) and
  `samtools_view` (0.1/1.0) to the type checker, validation and interpreter.
- **Compression (gzip/bzip2/zstd):** `src/compression.rs` (de)compresses gzip (`flate2`), bzip2
  (`bzip2` crate, pure-Rust `libbz2-rs-sys` backend) and zstd (`zstd` crate); output is
  content-equivalent (the suite compares decompressed data). samtools cannot read zstd/bzip2, so SAM
  inputs in those formats are decompressed to a plain SAM temp before `samtools sort`/`view`/conversion
  (`samtools_input`).
- **`--print-path EXEC`:** implemented in `src/lib.rs` (mirroring `PrintPathMode` in `Execs/Main.hs`
  and `findNGLessBin`/`checkExecutable` in `FileManagement.hs`). Since the Rust build embeds no
  binaries, the tool path is resolved from `$NGLESS_<TOOL>_BIN` (which must point at an executable
  file) or by searching `PATH`; unknown tool names error with `Unknown binary <name>.`, as in Haskell.
  This drives the samtools `check.sh` scripts that shell out to `$(ngless --print-path samtools)`.
- **Counting:** `count()` with all three annotation modes and all four normalizations
  (`src/count.rs` + `src/gff.rs`, mirroring `Interpretation/Count.hs` and `Data/GFF.hs`). The seqname
  annotator is built from the `@SQ` header (sorted by name, sizes from `LN:`); the functional-map
  annotator parses a MOCAT-style TSV (one annotator per feature column, sorted by tag, `feature:` prefix
  only when >1 feature, sizes summed from `@SQ` when `normalization={normed}`); the GFF annotator parses
  GFF3/GTF (per-attribute `=`-vs-space and comma-split values), reindexes feature ids to sorted order
  and resolves overlaps with `union`/`intersection_strict`/`intersection_non_empty` under
  `sense`/`antisense` strand rules. Multi-mappers follow `all1`/`1overN`/`unique_only`/`dist1` (the
  dist1 second pass distributes by size-normalized weight, or 1/N when zero); `scaled`/`fpkm` rescale
  over the totals excluding the `-1` bucket. Output uses Rust's shortest-round-trip `f64` `Display`,
  which matches double-conversion's `toShortest` for every value in the suite. `write()` of counts has
  `format={csv}` (tab→comma) and a manual `comment=` (`# ` prefix). The Haskell async/conduit pipeline
  is performance-only and is replaced by a serial whole-file pass. `countfile()`
  (`Interpretation/CountFile.hs`) references an existing counts TSV, reordering the data rows by tag
  when they are not already sorted while keeping leading comments and the sample-name header in place.
  `mapstats()` (`executeMapStats` + `samStatsC'`) summarizes a mapped read set as total/aligned/unique
  read groups (grouped by name, a group is *aligned* if any read aligns and *unique* if aligned and all
  records share one reference). `write()` of counts supports `auto_comments=[{script}]`: the original
  script source is threaded through the interpreter (`Interpreter::script_text`, mirroring
  `ngleScriptText`) and `build_comment` reproduces `buildComment` — a `Output generated by:` header
  followed by the verbatim script lines indented four spaces, after any manual `comment=`, each line
  `# `-prefixed. Still pending: `auto_comments={date}`/`{hash}` is partly handled (see M7d for
  `{hash}`); the double-conversion scientific switch for out-of-range exponents is deferred.
- **Mapping with bwa:** `map()` for the `fafile=` form with the bwa mapper (`src/mapper.rs` +
  `execute_map`/`perform_map` in `src/interpret.rs`, mirroring `Interpretation/Map.hs` and
  `StandardModules/Mappers/Bwa.hs`). `src/mapper.rs` shells out to the external `bwa` (`$NGLESS_BWA_BIN`
  or `bwa` on PATH), querying its version at runtime and embedding it in the index file names
  (`<base>-bwa-<ver>.<ext>`, mirroring `indexPrefix`/`bwaVersion`); indices are built lazily (`bwa
  index`, with the 1/10th-filesize `-b` block size for ≥100MB references) and reused via the
  `.amb/.ann/.bwt/.pac/.sa` set. Reads are interleaved for `bwa mem -K 100000000 -p` (`interleave_fastq`,
  mirroring `interleaveFQs`: pairs emitted record-by-record and re-normalised to LF, then singletons
  appended verbatim) and streamed on stdin while stdout is redirected to a SAM temp.
  `block_size_megabases=` splits the FASTA into block-sized chunks (`split_fasta`/`ensure_splits_exist`,
  mirroring `splitFASTA`); a single chunk maps directly, multiple chunks map separately and merge
  best-only (`merge_sam_files`/`merge_sam_group`, mirroring `mergeSAMGroups MSBestOnly` at ≥1.1).
  `fafile=` is resolved through `--search-path` with `<references>`-style placeholders
  (`expand_path`/`expand_path_candidates`, mirroring `expandPath'`). The minimap2 mapper is implemented
  too (`src/minimap2.rs`, `map(..., mapper='minimap2')`, activated by `import "minimap2"`): it shells
  out to `minimap2 -a`, then sorts the SAM exactly like `sortSam`. Still pending: the `soap` mapper.
- **Lock files:** `src/lockfile.rs` ports
  `Utils/LockFile.hs`. `acquire_lock(LockParameters)` atomically creates the lock file with
  `O_CREAT|O_EXCL` (`create_new`) and returns an RAII `LockGuard` that removes the file on drop; a lock
  older than `max_age` is reclaimed as stale, and `WhenExistsStrategy`
  (`IfLockedNothing`/`IfLockedThrow`/`IfLockedRetry`) reproduces the loser behaviour. With
  `mtime_update`, a background thread touches the file every 10 minutes so a long critical section is not
  mistaken for stale. `with_lock_file(params, act)` is the `withLockFile` wrapper. It is wired into the
  three expensive, cacheable side effects with the exact Haskell lock parameters: mapper-index creation
  (`ensure_one_index_exists` → `<fafile>.ngless-index.lock`, 36h max-age, 37×60 retries), FASTA
  splitting (`ensure_splits_exist` → `<fafile>.<N>m.split.lock`, 120 retries), and reference
  download/unpack (`install_data` → `<destdir>.download.lock`, 37×60 retries). The reference-download
  lock goes one step beyond Haskell, whose builtin `installData` path is unlocked, closing the
  concurrent-same-ref install race. The **parallel module's** `get_lock` (`lock1`/`run_for_all`) is
  unified onto this same primitive: it claims each candidate through `acquire_lock` (`IfLockedNothing`,
  1h max-age, `mtime_update` — matching Haskell's `getLock'`) and rechecks `.finished` under the lock.
  The claimed `LockGuard` is held in the interpreter (`held_locks`) for the run and released on drop.
  **`.finished`/`.failed` markers (replacing `FinishOkHook`/`registerFailHook`)** are done without a
  hook system: each `lock1`/`run_for_all` claim is recorded on the interpreter (`claimed_locks`), and
  after the script body completes `interpret()` writes a `<sane>.finished` receipt for every claim on
  success or a `<sane>.failed` log on error (best-effort, so a marker write never masks the run
  result). `get_lock` is a two-pass scan: it skips `.finished` entries and prefers never-attempted
  ones, falling back to retrying `.failed` entries last (mirroring `getLock`'s failed-retry pass; the
  contention-only `shuffleM` randomization is dropped). This is what makes the multi-run
  `tests/parallel_2` workflow pick a different sample on each run and collect once all are done.
- **Modules + stdlib — sample/directory loading:** `group()` is interpreted (`execute_group`,
  mirroring `executeGroup`/`groupFiles`). On top of it, `load_fastq_directory`
  (`BuiltinModules/LoadDirectory.hs`) globs `<dir>/*.{fq,fastq}{,.gz,.bz2,.xz}` (sorted), pairs files
  by the `matchUp` rules, loads singletons via `fastq` and pairs via `paired`, then `group`s them under
  the directory name. The `mocat` module (`StandardModules/Mocat.hs`) exposes `load_mocat_sample` as a
  thin wrapper over the same loader (versions `0.0`/`1.0`/`1.1`, with the MOCAT/MOCAT2 citations).
  `load_sample_list` (`BuiltinModules/Samples.hs`) parses a YAML sample manifest (`serde_yaml`) into a
  list of named, Sanger-encoded read sets, and `load_sample_from_yaml(yaml, sample=...)` returns the
  single read set with the requested name (`execute_load_sample`, mirroring `executeLoadSample`),
  erroring when the sample is not found. The `.name()` method on a read set and `discard_singles`
  (`AsReads.hs`) are done, as is the `--subsample` flag (deterministic 1/10 record sample on load —
  `subsample_text` mirrors `drop90`/`performSubsample`, plus the `.subsampled` write-output marker from
  `parseWriteOptions`).
- **Modules + stdlib — packaged reference databases:** `map(..., reference="...")` resolves a
  packaged reference to its FASTA and feeds it through the same indexing/mapping path as `fafile=`
  (`resolve_reference` in `src/interpret.rs` → `reference::ensure_data_present` in `src/reference.rs`,
  mirroring `ensureDataPresent`/`findDataFiles`/`installData`). The reference directory is named by the
  *user-typed* name — so `sacCer3` and its alias `Saccharomyces_cerevisiae_R64-1-1` resolve to separate
  directories — and is searched for at `References/<name>/Sequence/BWAIndex/reference.fa.gz` in the user
  data directory (`$HOME/.local/share/ngless/data`) first, then the global one, matching `findDataFiles`'s
  `User <|> Root`. The bwa index is (re)built lazily next to the cached FASTA. **A missing builtin
  reference is auto-downloaded** (`ureq` over HTTPS) from
  `<base>/References/<maj>.<min>/<canonical-name>.tar.gz` — the `<maj>.<min>` version directory and
  canonical-vs-user-typed name split exactly mirror `installData` — then gunzipped+untarred
  (`flate2`+`tar`) into the install dir (global if writable, else user, mirroring `canInstallGlobal`).
  The `builtinReferences` list (16 Ensembl genomes + aliases) is ported in `src/reference.rs`; the base
  URL defaults to `https://ngless-resources.big-data-biology.org/` and is overridable via
  `NGLESS_DOWNLOAD_BASE_URL`. `qcstats({mapping})` is also done: each `map()` call records `(total,
  aligned, unique)` read groups into a mapping-stats accumulator (`register_map_stats` + the shared
  `sam_group_stats`, mirroring `samStatsC'` and `outputMappedSetStatistics`), serialised to the
  transposed TSV (`format_map_stats_tsv`); its `inputFile`/`reference` fields hold temp/index paths
  exactly as Haskell records them, so the output is not byte-reproducible and no test diffs it. Not yet
  ported: the download progress bar, and URL-typed module references (Rust external modules carry no
  references yet).
- **Modules + stdlib — external YAML modules + `example`:** external `.ngm` modules are loaded
  and executed (`src/external_modules.rs`, mirroring `ExternalModules.hs`). `find_load` searches
  `.`/global/user for `Modules/<name>.ngm/<version>/module.yaml`, parses it with `serde_yaml`
  (functions, `arg1`, `additional` args, `init`, `min-ngless-version`, citations), checks the version is
  compatible, and `validate` runs the optional `init` command with the module environment.
  `functions_for_typecheck` exposes each command as a `Function` (mirroring `asFunction`). At run time,
  unknown function calls are dispatched to `execute_external_command` (mirroring `executeCommand`):
  `encode_command_arg` builds the command line exactly as `encodeArgument` does — the unnamed `arg1` is
  positional, named args become `--name=value`, `flag`s emit their `when-true` tokens (or `--name`) when
  true, defaults (`def`) fill omitted args, `str` args with `expand_searchpath` go through `<...>`
  search-path expansion, and a read-set argument is concatenated to mate-1/mate-2/singles temp files
  (`encode_readset_files`, mirroring `asFilePaths`). A non-void `return` adds a `--name=<tempfile>`
  output argument and decodes the result. cli.rs routes built-in standard modules
  (`samtools`/`mocat`/`example`) through `module_functions` and everything else through the external
  loader. The `example` standard module (`StandardModules/Example.hs`) is also ported, which required
  threading **module constants** (`EXAMPLE_HELLO`, ...) through the type checker (`checktypes` takes a
  `constants` list), validation, and the interpreter; `example()` prints its diagnostics including a
  faithful Haskell-`show` of the read set (`haskell_show`).
- **Modules + stdlib — the `batch` module:** the `batch` standard module
  (`StandardModules/Batch.hs` + `Utils/Batch.hs`) is ported in `src/batch.rs`. It contributes no
  functions but two constants describing the batch array-job index — `JOBINDEX_OR_0` (the index, or
  `0` when not under a batch scheduler) and `JOBINDEX_VALID` (whether one was found) — read from
  `LSB_JOBINDEX` (LSF) then `SGE_TASK_ID` (SGE) via `get_job_index`, wired through the same
  module-constant path as `example` (`module_constants`/`module_constant_values`). Loading it also
  reproduces the `setNumCapabilities` side effect: when the environment advertises a CPU allotment
  (`OMP_NUM_THREADS`/`NSLOTS`/`LSB_DJOB_NUMPROC`/`SLURM_CPUS_PER_TASK`, in order — `get_ncpus`), the
  worker thread count is overridden in the import loop (`cli.rs` → `parallel::override_n_threads`),
  superseding `--jobs` exactly as the Haskell module load supersedes the command-line value. The
  `parallel` thread config was moved from a write-once `OnceLock` to atomics so this late override is
  possible.
- **Modules + stdlib — the parallel module + output hashing:** the `parallel` standard module
  (`StandardModules/Parallel.hs`) is ported, along with the script transforms that feed it
  (`src/transform.rs`, mirroring `Transform.hs`). The headline piece is **output hashing**:
  `add_output_hash` injects a hidden `__hash` keyword argument into every `write`/`collect` call whose
  value is **byte-identical** to Haskell's `MD5.md5s . MD5.Str . (versionString ++) . show`. This
  required reproducing (a) a from-scratch MD5, (b) Haskell's derived `Show` for the AST
  (`show_expr`/`show_ngltype`), (c) the `versionString` = `show nv ++ show (sortOn modName modInfos)`
  over the exact list of loaded modules (`loaded_modules`, mirroring `Execs/Main.hs::loadModules`), and
  (d) the effect of the `addTemporaries` pass (nested non-void calls are lifted into temporaries). This
  is what `auto_comments=[{hash}]` reports as `# Output hash: <md5>`. The `parallel_transform` ports
  `processRunForAll` (the v1.1 `run_for_all` rewrite that saves `$parallel$iterator`/`$parallel$list`
  and injects `current`/`allneeded` into later `collect`s), `processSetParallelTag`, and `addLockHash`.
  At run time, `lock1`/`run_for_all` claim one list entry by creating a `<sane>.lock` file in a hash
  directory under `ngless-locks/` (`setup_hash_directory`/`get_lock`/`sanitize_path`); `collect` writes
  each sample's counts as a gzipped partial under `ngless-partials/` and, once all needed partials
  exist, merges them with `paste_counts` (the sparse index-keyed merge of `pasteCounts`) prefixed by
  the `{script}`/`{hash}` comment block; `__paste` exposes the same merge. (Both the partial and the
  final `collect` writes go through `compression::write_bytes_atomic` — temp + fsync + rename, mirroring
  `moveOrCopy` of a `syncFile`d temp. When not all partials are present, the "run ngless once per
  sample" guidance is emitted inline, gated on `--quiet`.) Still unported in `collect`: the
  `--subsample` `.subsample` ofile suffix and `auto_comments={date}`.
- **Modules + stdlib — `assemble` + `orf_find`:** the two external-tool wrappers are ported.
  `assemble` (`BuiltinModules/Assemble.hs`) concatenates the read set's mates/singletons (recompressed
  to gzip, which megahit accepts) into `-1`/`-2`/`-r` inputs and runs `megahit` (`NGLESS_MEGAHIT_BIN` or
  PATH) into a fresh output directory, returning the `final.contigs.fa` as a new
  `NGLessObject::SequenceSet`. `orf_find` (`BuiltinModules/ORFFind.hs`) runs `prodigal` (`-i`/`-d`, `-p
  meta` for `is_metagenome`, `-a` for `prots_out`, `-o /dev/null`/`-f gff` for `coords_out`, `-c` for
  `!include_fragments`) and returns the predicted-genes FASTA filename. The type checker already allowed
  the `NGLSequenceSet → NGLString` coercion these need; `write` copies a `SequenceSet`/`Filename` to its
  output, and `map(reads, fafile=contigs)` accepts a sequence set as the FASTA. megahit assembly is
  byte-identical to the Haskell output once the thread count matches (ngless defaults to `--jobs 1`, and
  megahit's result is thread-count dependent, so `--num-cpu-threads 1` reproduces the committed output).
- **`unique()` FASTQ function:** the read-dedup builtin (`Interpretation/Unique.hs`, `unique(reads,
  max_copies=N)`) is interpreted (`Interpreter::execute_unique` in `src/interpret.rs`). The pure core is
  `fastq::unique_reads<R: BufRead, W: Write>`, a single streaming pass keeping ≤`max_copies` reads per
  distinct sequence (via a `HashMap<sequence, count>`) written gzip-compressed to a fresh temp read set.
  This mirrors `executeUnique`/`performUnique`/`filterUniqueUpTo`: Haskell first hash-buckets the input
  into `k` temp files purely to bound memory and dedups each bucket independently, but since all copies
  of one sequence land in the same bucket the *set* of kept reads is identical to the single pass (for
  any sub-512MB file `k=1`, so the bucketed and single-pass outputs are byte-identical too). Only
  single-end read sets are handled (as in Haskell); a `NGOList` of read sets is mapped element-wise. NB
  the `{unique}` select condition and the `.unique()` mapped-read method are separate
  (`src/select.rs`).

## Known remaining gaps (full Haskell↔Rust comparison)

All 99 functional tests pass, so parity is complete for the script surface the suite exercises. The
items below are features present in the Haskell binary but absent (or simplified) in Rust; **none are
covered by `tests/`**. They are grouped by impact.

- **HTML/JS run report.** `Output.hs::writeOutputJS` writes a report directory (`output.js` + HTML) at
  end of run; `src/output.rs` has no report writer at all (only the console output layer).
- **Standard modules not ported:** `motus` (`StandardModules/Motus.hs`, deprecated motus1 wrapper)
  and the `soap` mapper (`StandardModules/Soap.hs`; registered on `import` but `execute_map` rejects
  it). Both are referenced only for hashing in `src/transform.rs`. (`batch` is now ported — see the
  stdlib section above.)
- **CLI sub-modes + flags — DONE (all phases 0–4).** A phased implementation plan lives in
  `next-steps.md` ("CLI flags & sub-modes — implementation plan"). `lib.rs` peels off the
  informational `infoOption`s (`--version*`/`--date-short`/`-h/--help`) and hands the rest to
  `cli::run_cli`, which dispatches on a `Mode` enum mirroring `CmdArgs.NGLessMode`
  (`Default`/`PrintPath`/`CheckInstall`/`InstallReferenceData`/`DownloadFile`/`DownloadDemo`/
  `CreateReferencePack`). Every mode is implemented: `Default` (the run/validate flow),
  `--print-path`, `--check-install`, the **Phase 2** download sub-modes `--install-reference-data
  REF`, `--download-file --download-url URL --local-file PATH` and `--download-demo NAME`, and the
  **Phase 4** `--create-reference-pack --output-name NAME --genome-url URL [--gtf-url URL]
  [--functional-map-url URL]` (the `Mode` variants carry their parsed arguments). The download modes
  reuse `src/reference.rs`'s now-`pub` `install_data`/`download_file`/`download_expand_tar`; each
  first calls `init_submode_config` (mirroring `initConfiguration` running before every `modeExec`)
  so the config-file `download-url`/data directories apply. `--install-reference-data` rejects a
  non-builtin ref with the exact Haskell text (`Reference <ref> is not a known reference.`) and
  installs at version `(1,5)` (`parseVersion Nothing`); `--download-demo` reproduces the
  known-list/`suggestionMessage` "Unknown demo" block and unpacks `<base>/Demos/<name>.tar.gz` into
  the current directory. **`--create-reference-pack`** calls `reference::create_reference_pack`
  (porting `createReferencePack`): `download_or_copy_file` (new; porting `downloadOrCopyFile`/`isUrl`)
  fetches the genome + optional gtf/functional-map into a temp `Sequence/BWAIndex`+`Annotation`
  layout, `mapper::create_index` bwa-indexes it, and `write_targz` packs the FASTA + the five
  `-bwa-<ver>.gz.{amb,ann,bwt,pac,sa}` index files (+ optional annotation files) into the gzipped
  tar — the archive layout matches Haskell byte-for-byte (verified end-to-end with a local genome).
  **Unknown flags error** (`Invalid option \`...'`, mirroring optparse) instead of being silently
  ignored; a bare `-` stays a valid STDIN positional.
  **Phase 3 — export modes (`--export-json`/`--export-cwl`, gated on `--experimental-features`)** are
  ported in `src/export.rs` and wired into `cli.rs::run_script` after the builtin transforms
  (mirroring the `whenJust (exportJSON…)`/`whenJust (exportCWL…)` block, each ending in
  `exitSuccess`). The validated (pre-transform) script is cloned as the "original"; JSON serialises
  both it and the post-transform script (`write_script_json`, porting `JSONScript.hs` faithfully via
  `serde_json` — the `Optimized` case is absent since the Rust AST lacks that variant), CWL uses only
  the original (`write_cwl`, porting `CWL.hs`). **Known divergence (D-A1 below):** `CWL.hs::
  extractARGVUsage'` matches `IndexExpression (Lookup _ (Variable "ARGV")) …`, and in Haskell `ARGV`
  *is* a `Lookup` (a module constant from `BuiltinModules/Argv.hs`), so the pattern **fires** and
  emits non-degenerate CWL `inputs:`. In Rust `ARGV` is a `BuiltinConstant` (`tokens.rs` `CONSTANTS`),
  never a `Lookup`, so `export.rs` matches nothing and emits degenerate/empty CWL — a real
  (untested) discrepancy, not parity.
  **Phase 1 wired the behaviour of the small `DefaultMode` flags** (mirroring `modeExec
  DefaultMode`):
  - **`-e/--script` (inline script)** — `run_script` selects file-vs-inline (`loadScript` + the
    `(fname, reqversion)` split): an inline script is named `"inline"`, carries **no** version
    requirement (no header ⇒ default version 1.5, mirroring `parseVersion Nothing`), and is **not**
    prepended to `ARGV` (`nConfArgv`: `ScriptFilePath f -> f:extraArgs`, else `extraArgs`). The file
    path `-` now reads the script from STDIN (`load_script_file`).
  - **`-p/--print-last`** — `transform::wrap_print` (porting `wrapPrint`) wraps the last statement in
    `write(<expr>, ofile=STDOUT)` right after parsing, before type-checking, so it is type-checked,
    validated and seen by `uses_STDOUT` (which suppresses the run header). It errors if the script
    already terminates in a `print`/`write` call.
  - **`--color auto|no|yes|force`** — overrides the config-file `color` key (`None` ⇒ fall through)
    before `output::init`.
  - **`--experimental-features`** — gates the export modes (its *only* effect): `--export-json`/
    `--export-cwl` without it produce the exact Haskell `fatalError`; *with* it they run the export
    (Phase 3, above).
  - **`--index-path`** — threaded into mapper index resolution. `ensure_one_index_exists`/
    `ensure_splits_exist` (`src/interpret.rs`) call the new `get_index_output` (porting
    `getIndexOutput`): when the index store path is set and the FASTA is not already under it
    (`is_sub_path`/`split_path`, faithful to `System.FilePath.splitPath`'s trailing-slash quirk), the
    index is mirrored into `<index-dir>/<abs-fafile-without-leading-slash>` with a symlink back to the
    original FASTA, and the bwa index is built there. A reference that already carries a valid index
    is used in place (the original location is checked first), so `--index-path` only redirects
    *missing* indices.
  All remaining `DefaultMode` flags are parsed into `RunOpts`: `--search-dir` (deprecated alias for
  `--search-path`), `--check-deprecation`, `--create-report`/`--no-create-report`,
  `-o/--html-report-directory`; the `keep-temporary-files`/`strict-threads`/`create-report`
  triSwitches accept their `--no-` forms (`Option<bool>`, `None` ⇒ fall through to config). Still
  pending: `--check-deprecation` (needs module deprecation metadata) and `--create-report`/`-o`
  (await the HTML report).
- **Config-file reader — DONE** (`src/configuration.rs`, mirroring `Configuration.hs`). The three
  Haskell config steps are reproduced: `guess_configuration` (environment defaults, including the
  `$XDG_DATA_HOME` user-directory resolution from `getDefaultUserNglessDirectory`),
  `read_config_files` (the default locations `$HOME/.config/ngless.conf`, `$HOME/.ngless.conf`,
  `/etc/ngless.conf` — all optional — plus any `-c/--config-file` files, which are required to exist;
  later files override earlier, so the CLI files win), and the command-line override step in
  `cli.rs::run_script` (`updateConfigurationOpts`: boolean flags can only turn an option on, so an
  absent flag falls through to the config value). The file format is the realistic subset of
  `Data.Configurator`: `#` line comments (quote-aware), `key = value` bindings whose value is a
  double-quoted string, a bare word, a `true`/`false` boolean, or a `[..]` string list; interpolation,
  `import`, numeric literals and groups are not supported (ngless reads none of those). The keys read
  are exactly those Haskell reads — `download-url`, `global-data-directory`, `user-directory`,
  `user-data-directory`, `temporary-directory`, `keep-temporary-files`, `strict-threads`, `color`
  (`auto`/`force`/`none`, with `yes`/`no` synonyms), `print-header`, `create-report`, `search-path`,
  `index-path` — note `jobs` is deliberately *not* read from the config (the Haskell binary reads
  `nThreads` from the command line only, despite the docs). The resolved config is published
  process-globally (`configuration::init_global`/`global`, mirroring the global `nglConfiguration`);
  `src/reference.rs` reads `download-url`/`global-data-directory`/`user-data-directory` from it
  (`NGLESS_DOWNLOAD_BASE_URL` still overrides the URL as a Rust-only convenience), and `--color`/the
  `color` key now flow into `output::init`. This unblocks `-c/--config-file` (also wired:
  `--index-path`). Not yet wired downstream: `index-path`/`create-report` are stored but unused until
  the index-store and HTML-report features land. The resolved configuration is also dumped under
  `--trace` (`cli.rs::output_configuration`, mirroring `outputConfiguration` in `Output.hs`): a
  `# Configuration` block of `Debug`-level lines (visible only with `--trace`) listing the download
  URL, data/temp directories, the boolean/colour/verbosity settings (rendered with Haskell's derived
  `Show` — `True`/`False`, `AutoColor`/`NoColor`/`ForceColor`, `Quiet`/`Normal`/`Loud`), the index
  storage path (when set) and the search path. For `AutoColor` the line additionally reports the
  resolved decision in parentheses (`AutoColor (color)`/`AutoColor (no color)`) so the effective
  behaviour of the terminal-status + `NO_COLOR` check is visible.
- **`writeToMove`/`addMove` — DONE** (`src/transform.rs::write_to_move`, wired in `cli.rs` as the
  first builtin transform, before `add_file_checks`). The last-use analysis walks the body tracking a
  `blocked` set (variables bound to `fastq`/`paired`/`samfile` or aliases of such — the user's input
  files, never movable) and injects `__can_move=True` into every `write(v)` whose `v` is not used in
  any later statement. At run time `execute_write` reads the flag and `move_or_copy_compress`
  (`src/interpret.rs`) renames (with a cross-device copy fallback, mirroring `moveOrCopy`) instead of
  copying — but only when the source shares the destination's compression format, the destination is
  not STDOUT, and the source is a temp file this run created (`TempFiles::was_created`, mirroring the
  `ifile elem createdFiles` guard in `moveIfAllowed`). All FASTQ/counts/sequence-set/mapped-read-set
  write paths route through it. This is output-neutral (a move and a copy produce the same destination
  bytes), so the `tests/` suite stays byte-identical.
- **Mid-run temp-file GC — DONE** (`gcTemps`/`removeIfTemporary` in `Interpret.hs`,
  `BuiltinModules/Remove.hs`). `Interpreter::gc_temps` runs before every top-level statement: it
  unions the backing files of all live global variables (`NGLessObject::backing_files`, mirroring
  `extractFiles`/`extractFilesRS`) and reclaims every temp file this run created that is *not* in that
  set, via `TempFiles::remove_if_temporary` (mirroring `removeIfTemporary` — respects
  `--keep-temporary-files`, only touches files the run created, untracks on removal). Only files are
  candidates (`created_files` excludes directories, matching Haskell tracking only `openNGLTempFile'`
  files in `ngleTemporaryFilesCreated`), so a temp dir backing e.g. a `SequenceSet` is never reclaimed
  out from under it. This frees intermediate disk space as soon as a value goes out of scope (disk-usage
  only — every reclaimed file is provably dead — so the suite stays byte-identical). The separate
  `__remove` builtin *function* (a manually-invokable hook, not injected by any transform pass) shares
  the same `remove_if_temporary` primitive; it is unused by `tests/` and not wired as a callable yet.
- **`Transform.hs` passes not ported** (Rust applies `add_output_hash`+`addTemporaries`,
  `parallel_transform`, `write_to_move`, `add_file_checks`):
  the output-neutral optimizations
  `qcInPreprocess`/`ifLenDiscardSpecial`/`substrimReassign` and the early-check injections
  `addRSChecks`/`addIndexChecks`/`addCountsCheck` (Rust does eager IO validation differently, so these
  are partly covered). `addUseNewer` is correctly out of scope at ≥1.5.
- **Reference-download path:** `count(reference=...)` annotation download still errors (`src/interpret.rs`:
  "automatic annotation download is not supported"). The download machinery now exists; the gap is that
  `ensure_data_present` only surfaces the FASTA path, whereas Haskell's `ensureDataPresent` also returns
  `rfpGffFile` — extend it to surface the GFF (`Annotation/annotation.gtf.gz`)/functional-map paths.
  URL-typed module references unsupported (`moduleDirectReference`/`ExternalPackagedReference`; external
  modules carry no `references:` section in their YAML); download progress bar is silent (Haskell prints
  `mkProgressBar` while streaming; `ureq` exposes `Content-Length`, so a bar is straightforward).
- **Per-position quality percentiles** (`qualityPercentiles` in `Data/FastQ.hs`) are simplified out of
  the Rust QC accumulator.
- **Numeric:** `count()` normalization output uses Rust `{}` float formatting (`src/count.rs`
  `to_shortest`), which never switches to scientific notation for exponents `< -6` or `>= 21`, unlike
  Haskell's double-conversion `toShortest`. Reachable via `normalization={normed}` on multi-megabase
  contigs.
- **`collect()` "cannot collect" guidance timing.** The `.finished`/`.failed` markers themselves are
  now ported (see the parallel-module section); the one nuance left is that `collect`'s "run ngless
  once per sample" guidance is emitted inline rather than deferred to end-of-run as Haskell's
  `FinishOkHook` does (gated on `--quiet`, not output-affecting).
- **`collect()` minor gaps:** the `--subsample` `.subsample` ofile suffix and `auto_comments={date}`
  remain unported.

### Full module-by-module diff — behavioral discrepancies (untested)

A direct Haskell↔Rust module-by-module comparison (front end, type checker, validation, counting,
FASTQ/SAM, select/map, transforms, modules, CLI, config, output) turned up the items below. All 99
functional tests still pass, so each item is either outside the tested surface or output-neutral. The
three most likely to bite real users are **D-A1** (`ARGV` idiom → hash/CWL/JSON divergence), **D-A2**
(now ported — see below), and **D-A4** (silently missing citations).

- **D-A1 — `ARGV` node kind differs (output-affecting).** In Haskell `ARGV` is a module constant
  (`BuiltinModules/Argv.hs`) so it parses to `Lookup (Variable "ARGV")`; in Rust it is listed in
  `tokens.rs` `CONSTANTS` → `BuiltinConstant(Variable("ARGV"))`. Divergences where the idiom
  `fastq(ARGV[1])` feeds a hashed/exported value: (a) **output hash** — Haskell's `hashOf` shows the
  use as `Lookup 'ARGV' as NGList NGLString`, Rust as `ARGV`, so `auto_comments=[{hash}]` and
  `collect`'s `# Output hash:` differ (the `varmap.insert("ARGV","ARGV")` seed in `transform.rs` is
  inert since ARGV is never a `Lookup` in Rust); (b) **`--export-cwl`** fires `extractARGVUsage'` in
  Haskell (non-degenerate `inputs:`) but never in Rust (empty); (c) **`--export-json`** serialises the
  node differently (Lookup vs BuiltinConstant). No test combines ARGV with hashing/collect/export.
- **D-A2 — `load_sample_from_yaml` — DONE.** `BuiltinModules/Samples.hs` registers **two**
  functions — `load_sample_list` and `load_sample_from_yaml` (min version 1.5, required `sample`
  kwarg). Both are now in Rust: `modules.rs` registers `load_sample_from_yaml` and `interpret.rs`
  dispatches it to `execute_load_sample` (parse the manifest via `execute_load_sample_list`, return
  the read set whose name matches `sample`). Covered by `tests/load_sample_from_yaml`.
- **D-A3 — samtools `sortOFormat` transform not ported → non-byte-identical BAM.**
  `StandardModules/Samtools.hs`'s `modTransform = sortOFormat >=> checkUnique`; `sortOFormat` rewrites
  `samtools_sort` to emit BAM directly (`__output_bam=True`) when its result is only used in a BAM
  `write`. Rust always sorts to SAM (`interpret.rs`, hard-coded `"sam"`) and converts to BAM at write
  time, so `x = samtools_sort(...); write(x, ofile='out.bam')` carries an **extra `@PG` line** (sort +
  view) vs Haskell's single one, plus an extra conversion pass. Tests pass only because their
  `check.sh` strips `@PG` (`grep -v '^@PG'`). NB `checkUnique` was also moved from a *transform*
  (Haskell) to the *validate* phase (`validation.rs::validate_select_unique_not_sorted`) — same error
  text, earlier timing.
- **D-A4 — run-header citations missed for nested `map`/`assemble`/`orf_find`.** `collectCitations`
  (both builds) matches only top-level `Assignment _ (FunctionCall f)`. Haskell runs it on the
  **transformed** script, where `addTemporaries` has lifted nested calls into `temp$N = <call>`, so
  nested calls are found; Rust runs `collect_citations` on the **pre-transform** script (`cli.rs`), so
  a nested `map`/`assemble`/`orf_find` is missed (e.g. `write(orf_find(contigs,…))` lists only
  megahit, not prodigal + BWA). Fix: run `collect_citations` after transforms, or recurse into
  sub-expressions. Untested (no `expected.stdout.txt` on the relevant test).
- **D-A5 — `count()` with both `gff_file` and `functional_map`.** `Count.hs::parseAnnotationMode`
  errors ("Cannot simultaneously pass a gff_file and an annotation_file"); Rust `parse_count_opts`
  silently prefers `functional_map`. User-error edge, untested.
- **D-A6 — `addIndexChecks` not ported → different out-of-bounds message + timing.** For
  `array[<constInt>]` Haskell floats a `__check_index_access` up to just after the array assignment,
  failing **early** with `Index access on line N is invalid.\n …`; Rust has no such check, so the
  error only surfaces when the index is evaluated (later, possibly after side effects) with the
  plainer runtime `Accessing element K in list of size M.`.
- **D-B — error-handling / leniency on edge inputs (untested):** (B1) malformed GFF — an empty
  attribute field or bare word crashes Haskell (`B.tail ""`), Rust skips gracefully (`gff.rs`); Haskell
  validates the score/phase columns, Rust drops them, so Rust accepts GFF lines Haskell rejects. (B2)
  functional-map empty data line — Haskell throws "wrong number of columns", Rust skips. (B3) encoding
  auto-detect — empty quality line or non-multiple-of-4 file crashes/errors in Haskell, Rust
  skips/ignores the tail; missing "Input file is empty"/"cannot be 100% confident" stderr warnings.
  (B4) `read[<int>]` single index on a ShortRead — Haskell yields a degenerate `srSlice(a+1,-1)`, Rust
  errors `_evalIndex: invalid operation`. (B5) SAM integer field as last token with no trailing tab —
  Haskell crashes (`B.tail ""`), Rust returns `""` (not reachable in well-formed SAM). (B6)
  undefined-variable wording/path differs (type-checker abort `` Could not find variable `x` `` vs
  Haskell's validator `` `"x"` ``). (B7) integer literals — Haskell uses arbitrary-precision `Integer`,
  Rust `i64`, so a huge literal panics in Rust (`.parse().unwrap()` in `tokens.rs`).
- **D-C — missing stderr warnings (not output-affecting):** `count()` omits the "both `strand` and
  `sense` … ignored" and "both `norm` and `normalization` …" warnings; assorted version-gated
  trace/warning lines (`select`/`filter`/`allbest`/SAM-merge) are moot or emitted differently at ≥1.5.
  All stderr, not diffed.
- **Verified matching (no action):** version strings, citation-header 90-column wrap, `show_double`,
  the SAM 11-column `qual→extra` quirk, all trimming (incl. round-half-to-even), the count
  annotation/normalization/multi-mapper math, GFF intersection modes, and select/filter/allbest logic
  were checked in detail and match. (`src/lib.rs`'s doc comment saying "type checker … not implemented
  yet" is stale — they are fully wired.)

## Context

NGLess is a ~15–16k-line Haskell program (`NGLess/`, 86 `.hs` files) implementing a version-pinned DSL
for NGS/metagenomics workflows, plus ~1.6k lines of unit tests and 97 functional tests under `tests/`
(the pre-1.5 cases have been removed). The motivation for a Rust rewrite is **not** performance —
Haskell/conduit already streams large files fine. The drivers are:

- **Build/maintenance pain** — GHC + Stack + Nix + `haskell.nix` materialized deps is a heavy, slow,
  hard-to-onboard toolchain; the Haskell contributor pool is small.
- **Contributors/ecosystem** — Rust has a larger contributor base and a strong bioinformatics crate
  ecosystem (`noodles`, `needletail`, `rust-bio`).
- **Distribution** — simpler single static binary and image builds (today this works but goes through
  Nix/musl/embedded-C machinery).

Decision taken (and since carried out): **full big-bang rewrite** aiming at behavioral parity,
then cut over. The cutover happened at the 1.6 release, at which point the Haskell sources were
removed.

**Scope decision: only `ngless "1.5"` and newer are supported.** Scripts declaring older versions
(`0.5`–`1.4`) do not need to run on the Rust binary — they can be rejected with a clear "version no
longer supported; use the Haskell build or update your script" error. This removes the entire
version-aware semantics long tail (the single largest risk), and lets us delete/skip the
version-specific functional tests rather than reproduce historical quirks. The cost is a hard break for
old scripts in the wild — acceptable given the 1.5 cutover is itself a major version.

The non-negotiable success criterion is **reproducibility at version ≥ 1.5**: the new binary must
produce byte-identical (or semantically identical, where tool versions differ) output to the Haskell
one for current scripts. The existing `tests/` suite is the contract that proves this.

**Version bump to 1.6 (Rust cutover release).** The Rust build is released as language version
**1.6**, a best-effort exact mirror of 1.5. The gate lives in `cli.rs`: `1.6` is the native version,
`1.5` still runs but emits a deprecation warning (`output::warn`), and anything `< 1.5` or `> 1.6` is
rejected. Both `1.5` and `1.6` normalise to the same `EFFECTIVE_VERSION` `(1,5)` for feature gating
and output hashing, so runs are byte-identical regardless of which of the two is declared. The Haskell
build treats `1.6` identically to `1.5` (`parseVersion (Just "1.6") = NGLVersion 1 5` in
`NGLEnvironment.hs`), so the two implementations agree on 1.6. All functional-test scripts now declare
`ngless "1.6"` (the `{script}` auto-comment echoes in the corresponding `expected.*` files were updated
in lockstep; the hashes are unchanged because the effective version and body AST are unchanged). User
docs for the switch live in `docs/sources/rust.md` (+ `whatsnew.rst`/`backwards.md`).

The **binary/tool version** was bumped to `1.6.0` (`src/lib.rs` `VERSION_STR`, `Cargo.toml`, and on the
Haskell side `Version.hs`/`package.yaml`/`NGLess.cabal`; release date `1 July 2026`). The run-header
banner (`NGLess v1.6.0 (C) NGLess authors`) is diffed by several `expected.stdout.txt` files, which
were updated. The **built-in standard modules** now treat `"1.6"` as their canonical version
(`modules::CURRENT_MODULE_VERSION`): `samtools "1.6"` and `parallel "1.6"` map to the latest behaviour
(same as `1.0`/`1.1` respectively), and the `cli.rs` import loop emits a deprecation warning when an
internal module is imported at any other version (stderr only, so no `expected.*` diffs; test scripts
keep their historical module versions, which now warn). The Haskell modules accept `"1.6"` too
(`Samtools.hs`/`Parallel.hs`).

## The single biggest asset: the functional test suite as a parity oracle

`tests/` (97 `.ngl` scripts + `expected.*` outputs, driven by `run-tests.sh`) is **language-agnostic**
— it runs a binary and diffs outputs. The entire rewrite is driven test-first against this suite:

1. Keep `tests/` and `run-tests.sh` unchanged.
2. Point `run-tests.sh` at the Rust binary via an env var (`NGLESS_BIN`).
3. Drop/skip the version-specific tests for `0.5`–`1.4`; keep only `ngless "1.5"`+ cases as the parity
   contract.
4. "Done" for any milestone = the corresponding subset of `tests/` passes against the Rust binary with
   identical output to the Haskell binary.

## What carries over cheaply vs. what is genuinely hard

**Cheap / mechanical:**
- AST and type enums (`Language.hs`, 286 lines) → Rust `enum`s with `serde` derives.
- Type checker (`Types.hs`, 407 lines) → straightforward single-pass inference; accumulate errors in a
  `Vec` instead of the `Writer` monad.
- Validation passes (`Validation.hs`, 360 lines) → list of pure functions over the AST.
- Error model (`NGError.hs`) → `enum NgErrorType` + `thiserror`/`anyhow`.
- Module YAML loading (`ExternalModules.hs`) → `serde_yaml` structs.
- External tool invocation — bwa/samtools/minimap2/megahit/prodigal are **already subprocesses**; this
  is a `std::process`/`tokio::process` wrapper, not a port. Keep the pinned versions in
  `Dependencies/Versions.hs` as a Rust constants module.

**Genuinely hard / risk-bearing:**
- **Conduit streaming model** (`Interpret.hs`, `Utils/Conduit.hs`, `Data/FastQ.hs`,
  `Interpretation/FastQ.hs`) — bounded async queues (`TBMQueue`), batched vectors, async gzip. Must be
  re-expressed with Rust channels/iterators without blowing up memory or changing output ordering.
- **Feature counting** (`Interpretation/Count.hs`, 988 lines — the largest subsystem) — annotation
  modes, overlap resolution, multi-mapper handling, normalization (raw / fpkm / scaled). Subtle and
  heavily tested; port last and lean hard on `tests/count*`.
- **FileOrStream abstraction** (`FileOrStream.hs`) — the file-vs-lazy-stream duality threads through
  everything; needs a clean Rust equivalent (an enum yielding a boxed iterator) designed up front.
- **Temp-file lifecycle / GC — exclusive creation + exit cleanup DONE.** Haskell's `ResourceT` +
  laziness drive cleanup of intermediate files. Rust now has explicit ownership via `src/tempfiles.rs`
  (`TempFiles`): every temp file/dir is allocated through the registry, created **exclusively**
  (`O_EXCL` / exclusive `create_dir`) with a length-checked name (porting `checkFilenameLength`),
  tracked, and removed when the `Interpreter` (which owns the registry) is dropped — on success, error,
  or panic-unwind — unless `--keep-temporary-files`. This mirrors `openNGLTempFile'` + the `ResourceT`
  cleanup. **Still open:** the mid-run GC (`removeIfTemporary`/`writeToMove`/`addMove`, disk-use only)
  and the script-hash-based output caching in `Transform.hs`.

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

Note this **eliminates all bundled C/C++/FFI** (`FastQ.c`, `RefSeqInfoVector.h`, `embedded.c`) — a real
maintenance win aligned with the stated motivation.

## Critical files to mirror (highest leverage)

- `NGLess/Language.hs`, `Types.hs`, `Parse.hs`, `Tokens.hs`, `Validation.hs`, `Transform.hs` — the
  front end (ignore the `< 1.5` version branches within them).
- `NGLess/Interpret.hs`, `Utils/Conduit.hs`, `FileOrStream.hs` — runtime + streaming core.
- `NGLess/Interpretation/Count.hs` — largest and most parity-sensitive.
- `NGLess/Data/{FastQ,Sam,GFF,Fasta}.hs` — replaced by noodles/needletail.
- `NGLess/ExternalModules.hs`, `Modules.hs`, `StandardModules/Parallel.hs` — module system.
- `NGLess/Dependencies/Versions.hs` — pinned external tool versions (copy verbatim).
- `run-tests.sh`, `tests/` — the acceptance oracle (do not change semantics).

## Risks & honest assessment

- **Parity over performance:** since speed isn't the goal, resist "improving" behavior mid-port — any
  semantic change that fails `tests/` is a regression, not a feature. Bank improvements for *after*
  parity is reached and the Haskell binary is retired.
- **Streaming correctness** is the top technical risk: bounded-memory behavior on multi-GB inputs and
  stable output ordering must be validated on large real inputs, which `tests/` (small fixtures) won't
  catch. Add a few large-input soak tests.
- **Reproducibility across tool versions:** outputs depend on bwa/samtools versions. Pin identical tool
  versions during the parity phase so diffs reflect *NGLess* behavior, not tool drift.

## Verification

Run the test suite with:

```
pixi run --environment default bash -c 'NGLESS_BIN=$PWD/target/debug/ngless ./run-tests.sh'
```

(Exit code 0 + "All done." = clean. Append a test-name prefix to run a subset, e.g.
`./run-tests.sh map-minimap2`. Unit tests: `cargo test`.)


- `cargo test` for ported unit tests (parse/type/count/validation), plus `proptest` for the
  tokenizer/parser round-trips.
- Large-input soak tests (multi-GB FASTQ/BAM) to confirm bounded memory and ordering.
