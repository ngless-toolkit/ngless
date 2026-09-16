# NGLess Developer Guide

This guide is for contributors working on the NGLess codebase.

**Language**: Rust (edition/toolchain: stable; formatting enforced with `cargo fmt`)
**License**: MIT
**Build tool**: Cargo (`Cargo.toml` at the repository root)

> **Status note**: NGLess was rewritten from Haskell to Rust. As of the 1.6 release the Haskell
> implementation was *completely removed* and the Rust code at the repository root is the sole,
> supported implementation. Many doc comments in `src/` are **stale** — they still describe features
> as "deferred to a later milestone" or "a scaffold" even where the Rust code fully implements them.
> Do not trust those; treat the Rust code as the full implementation. `rust-migration.md` is the most
> accurate status document. Comments referring to Haskell modules (`NGLess/Interpret.hs`, ...) are
> provenance pointers to the removed implementation, not to code you can read in this repo.

---

## Building & Testing

```sh
cargo build --release          # produces target/release/ngless
cargo test                     # unit tests (in-module #[cfg(test)])
cargo test <name>              # run a single unit test by name
cargo fmt --all -- --check     # formatting is enforced in CI
```

Functional test suite (the primary correctness bar):

```sh
NGLESS_BIN=target/release/ngless ./run-tests.sh              # all ~108 tests
NGLESS_BIN=target/release/ngless ./run-tests.sh regression   # only tests/regression*
```

**External tools** (samtools, bwa, minimap2, prodigal, megahit) are NOT bundled. The binary finds
them on `$PATH` or via per-tool overrides `NGLESS_SAMTOOLS_BIN`, `NGLESS_BWA_BIN`,
`NGLESS_MINIMAP2_BIN`, `NGLESS_PRODIGAL_BIN`, `NGLESS_MEGAHIT_BIN`. `pixi.toml` pins these tools; CI
and local runs get them via pixi:

```sh
pixi run --environment default bash -c 'NGLESS_BIN="$PWD/target/release/ngless" ./run-tests.sh'
```

CI is `.github/workflows/build_rust.yml` (fmt check → release build → unit tests → functional tests
under pixi).

---

## ChangeLog

Update `ChangeLog` for any user-visible change (new/changed/removed functions, flags, output, bug
fixes) and for large internal changes. Add a bullet under the current unreleased version block at the
top, matching the existing tab-indented `* ...` style.

---

## Execution Pipeline

The pipeline is, in order:

**load → tokenize → parse → version gate (== 1.6) → type check → validate → transform → interpret**

Entry: `src/main.rs` → `lib.rs::run` (handles `--version*`, `--help`, `--check-install`,
`--print-path`, `--debug-parse`) → `cli.rs::run_cli` / `run_script` drives the pipeline above.

---

## Source Layout

```
src/
  main.rs               — thin binary entry point
  lib.rs                — run(), informational options, version strings, tool path resolution
  cli.rs                — argument parsing, mode dispatch, run_script (the pipeline driver);
                          LANGUAGE_VERSION lives here

  Front end
  tokens.rs             — tokenizer
  ast.rs                — AST types (Expression, FuncName, NGLType, Block, ...)
  parser.rs             — hand-written recursive-descent parser over the token stream
  types.rs              — type inference/checking; annotates each Lookup with its type
  validation.rs         — pure validation passes (semantic checks beyond typing)
  modules.rs            — builtin function/method signatures and argument checks (ArgCheck),
                          module functions/constants, NGLVersion, CURRENT_MODULE_VERSION
  external_modules.rs   — user .ngm external module loading (see Modules/)

  Transform & run
  transform.rs          — post-typecheck AST transforms (incl. add_output_hash)
  interpret.rs          — the interpreter (largest module, ~6k lines)
  values.rs             — runtime values (NGLessObject), operators, show_double

  Domain subsystems
  fastq.rs              — file-backed read sets, streaming FASTQ records, QC stats
  compression.rs        — transparent gzip/bzip2/xz/zstd/plain I/O (open_read, StreamWriter)
  sam.rs                — SAM/BAM record parsing
  mapper.rs             — bwa mapping
  minimap2.rs           — minimap2 mapping
  samtools.rs           — samtools invocation (sort/view, BAM streaming)
  count.rs              — feature counting
  gff.rs                — GFF/GTF parsing
  select.rs             — SAM record filtering
  reference.rs          — reference-database resolution / auto-download (tar + HTTP via ureq)
  parallel.rs           — in-process compute parallelism: --jobs/--threads config and
                          par_map_ordered (order-preserving bounded parallel map)
  batch.rs              — the `batch` standard module: scheduler env vars (LSB_JOBINDEX,
                          SGE_TASK_ID, SLURM_CPUS_PER_TASK, ...)

  Cross-cutting
  configuration.rs      — config-file + env + CLI settings
  lockfile.rs           — lock files (parallel module)
  tempfiles.rs          — temp-file management
  cleanup.rs            — signal-driven cleanup (removes locks/temps on Ctrl+C / SIGTERM)
  output.rs             — run header, verbosity, transient messages
  citations.rs          — citation collection
  errors.rs             — error types
  suggestion.rs         — "did you mean" suggestions
  progress.rs           — single-line terminal progress bar with ETA
  report.rs             — HTML run report generation
  report/template.html  — the self-contained report template
  export.rs             — --export-json / --export-cwl
```

Note that the `.ngl` `parallel` module itself (`lock1`, `run_for_all`, `collect`) is implemented
across `interpret.rs`/`modules.rs`/`transform.rs`, not in `parallel.rs` (which is about thread-level
parallelism).

---

## Key Abstractions

### NGLessObject (`values.rs`)

The runtime value type:
- Scalars: `String`, `Bool`, `Integer`, `Double`, `Symbol`, `Filename`, `Void`, `List`
- `Read(ShortRead)` — a single read
- `ReadSet { name, readset }` — file-backed read set; the data lives in files on disk
- `SequenceSet(path)` — FASTA sequence set (e.g. `assemble` output)
- `MappedReadSet { name, path, reference }` — SAM/BAM file on disk; `reference` carries the packaged
  reference name (used as `count()`'s default `reference=`)
- `Counts(path)` — counts/statistics TSV on disk

### Errors (`errors.rs`)

```rust
pub enum NgErrorType { ShouldNotOccur, ScriptError, DataError, SystemError, GenericError, NoErrorExit }
pub struct NgError { pub kind: NgErrorType, pub message: String }
pub type NgResult<T> = Result<T, NgError>;

NgError::script("...")            // user script errors
NgError::should_not_occur("...")  // internal bugs
NgError::new(NgErrorType::DataError, "...")
```

Error kinds map to exit codes; the functional tests in `tests/error-*` pin that behaviour.

### The output hash (`transform.rs`)

`add_output_hash` computes an MD5 content hash injected as a hidden `__hash` argument (reported by
`auto_comments=[{hash}]`, and used to name parallel lock/stats directories). It is computed from the
`Debug` serialization of the rewritten AST. It only needs to be deterministic and content-addressed —
its exact value is internal and may change. If you change how it is computed, regenerate the handful
of `expected.*` files that embed it (`tests/write-hash*`, `tests/same-hash-collect*`) after confirming
the only diff is the hash line.

---

## Output Stability

The Rust build is the sole implementation, so **byte-for-byte parity with the removed Haskell binary
is no longer a goal**. The `expected.*` files are still the regression gate: run the functional suite
whenever you change anything that affects output, and keep output *stable across Rust releases* unless
a change is intentional.

Several code paths still reproduce Haskell's exact formatting (numeric output via `values::show_double`,
version/header strings, citation ordering, error exit codes) simply because the committed baselines
were produced that way and there is no reason to churn them — a convenience, not a hard constraint.

---

## Adding a New Builtin Function

1. **Define the signature** in `src/modules.rs`, in `builders::builtin_functions()`: name, unnamed
   argument type, return type, keyword arguments (`ArgInformation` with `ArgCheck`s such as
   `Symbol([...])`, `FileReadable`, `FileWritable`), and function-level checks (e.g.
   `FunctionCheck::ReturnAssigned` for pure functions whose result must be assigned).
2. **Implement it** in `src/interpret.rs` (add a case to the function-dispatch match), delegating the
   real work to a domain module (`count.rs`, `fastq.rs`, `select.rs`, ...) when it is more than a few
   lines.
3. **Add validation** in `src/validation.rs` if there are cross-argument constraints.
4. **Add a functional test** under `tests/` and a `ChangeLog` bullet.

### Adding a Standard Module Function

Standard modules are data in `src/modules.rs`: extend `builders::module_functions(name, version)` (and
`module_constants` for constants), then implement the function in `interpret.rs` as above. Runtime
values for module constants live in `interpret::module_constant_values`.

---

## Functional Test Structure

`run-tests.sh` iterates `tests/*/`. Each test directory contains one or more `*.ngl` scripts and
committed `expected.*` files; the harness runs ngless and `diff`s actual output against `expected.*`.
A directory may also contain:

- `cmdargs` — extra CLI arguments
- `run.sh` — custom invocation (replaces the default `ngless --quiet -t temp ... *.ngl`)
- `check.sh` — extra assertions
- `cleanup.sh` — cleanup after the test
- `TRAVIS_SKIP` — skip in CI

Naming conventions: directories named `error-*` expect a non-zero exit; `error-validation-*` are run
with `-n` (validate only).

Most `expected.*` files were originally produced by the old Haskell binary; the suite is the regression
gate for output, but exact Haskell parity is no longer a goal.

---

## External Module YAML Format

External modules live in `Modules/<name>.ngm/<version>/module.yaml` (searched in the repository, the
global data directory and the user data directory). They can add references and/or command-line
functions.

```yaml
name: 'my-module'
version: '1.0.0'
references:
  - name: 'my-ref'
    fasta-file: 'data/ref.fna'
    gtf-file: 'data/ref.gtf.gz'    # optional
    map-file: 'data/ref.map'       # optional functional map
functions:
  - nglName: 'my_function'
    arg0: './run.sh'
    arg1:
      atype: 'readset'             # readset/mappedreadset/counts/sequenceset/str/flag/int/option
      filetype: 'fq1'              # fq1/fq2/fq3/sam/bam/sam_or_bam/tsv
      can_gzip: false
      can_stream: false
    additional:
      - name: 'output_path'
        atype: 'str'
        required: true
      - name: 'verbose'
        atype: 'flag'
        def: false
        when-true: '-v'
      - name: 'mode'
        atype: 'option'
        def: 'fast'
        allowed: ['fast', 'careful', 'thorough']
    return:
      rtype: 'counts'              # void/counts/mappedreadset
      name: 'ofile'
      extension: 'tsv'
citations:
  - "Author et al. (2024). Title. Journal."
min-ngless-version:
  min-version: "1.3"
  reason: "Uses feature X"
```

References may also be declared as `packaged` reference packs (with `name-version` and `url`), which
are downloaded on first use. Module-declared references are usable from `map(..., reference=...)` and
`count(..., reference=...)`.

**Import rules**: a plain `import` is only accepted for *known* modules (`KNOWN_MODULES` in
`external_modules.rs`: `example-cmd`, `gmgc`, `igc`, `om-rgc`, `DogGutCatalog`, `MouseGutCatalog`,
`PigGutCatalog`, `specI`, `motus`), which are auto-downloaded when not present locally. Any other
module requires `local import`.

**Environment for module commands**: `NGLESS_MODULE_DIR` is set to the module directory, and
`NGLESS_NR_CORES` to the configured worker thread count (`--jobs`/`--threads`, or the `batch`
scheduler's allotment).

---

## External Tools

NGLess does **not** embed tool binaries. It resolves each tool from an environment variable or `$PATH`:

| Tool | Override | Used for |
|---|---|---|
| samtools | `NGLESS_SAMTOOLS_BIN` | SAM/BAM handling |
| bwa | `NGLESS_BWA_BIN` | read mapping (default mapper) |
| minimap2 | `NGLESS_MINIMAP2_BIN` | alternative mapper |
| prodigal | `NGLESS_PRODIGAL_BIN` | ORF finding |
| megahit | `NGLESS_MEGAHIT_BIN` | assembly |

`ngless --check-install` verifies the installation; `ngless --print-path <tool>` prints the resolved
path. `pixi.toml` pins versions for testing.

Other relevant environment variables: `NGLESS_DOWNLOAD_BASE_URL` (override the reference download
server), `NGLESS_MODULE_DIR` and `NGLESS_NR_CORES` (set for external module commands).
