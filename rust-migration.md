# NGLess Rust: known discrepancies vs. Haskell parity

The following are the remaining behavioral differences between the Rust implementation and the
Haskell parity target for `ngless "1.5"`+ scripts.

- **HTML/JS run report.** Haskell's `Output.hs::writeOutputJS` writes a report directory
  (`output.js` + HTML) at end of run; `src/output.rs` has no report writer (only the console output
  layer).

- **`Transform.hs` passes: only two edges remain.** Almost all passes are ported
  (`transform.rs`): `writeToMove`, `qcInPreprocess`, `ifLenDiscardSpecial`, `substrimReassign`,
  `addFileChecks`, `addRSChecks`, `addIndexChecks`, `addCountsCheck`, `addOutputHash`, and the
  parallel/samtools module transforms. The four `genericCheckUpfloat`-based checks (`addFileChecks`,
  `addRSChecks`, `addIndexChecks`, `addCountsCheck`) share one `generic_check_upfloat` port;
  `addCountsCheck` injects a runtime `__check_count` (`interpret.rs::execute_count_check`) that catches
  missing `functional_map` features for *dynamic* count arguments — the static-argument case is
  already covered eagerly by `validation.rs::validate_count` (see `tests/error-count-check-dynamic`).
  `qcInPreprocess` and `substrimReassign` are output-neutral optimizations, ported faithfully
  (`transform.rs::qc_in_preprocess`/`substrim_reassign`, with interpreter support for the hidden
  `__perform_qc`/`__input_qc` args and the `Optimized(SubstrimReassign …)` node). Two edges are left:
  **`addTemporaries`** is not ported — the only observable gap is `<call>()[<constInt>]` (indexing a
  call result directly, without binding it to a variable); see the out-of-bounds note below.
  **`addUseNewer`** is out of scope at ≥1.5 (it only fires below `ngless "1.1"`).

- **Reference-download path:** `count(reference=...)` annotation download errors (`src/interpret.rs`:
  "automatic annotation download is not supported"). `ensure_data_present` only surfaces the FASTA
  path, whereas Haskell's `ensureDataPresent` also returns `rfpGffFile`; it needs to surface the GFF
  (`Annotation/annotation.gtf.gz`) / functional-map paths. URL-typed module references are
  unsupported (`moduleDirectReference`/`ExternalPackagedReference`; external modules carry no
  `references:` section in their YAML).

- **Per-position quality percentiles** (`qualityPercentiles` in `Data/FastQ.hs`) are simplified out
  of the Rust QC accumulator.

- **`collect()` minor gap:** the `--subsample` `.subsample` ofile suffix is unported.

- **`ARGV` node kind differs (output-affecting).** In Haskell `ARGV` is a module constant
  (`BuiltinModules/Argv.hs`) so it parses to `Lookup (Variable "ARGV")`; in Rust it is in `tokens.rs`
  `CONSTANTS` → `BuiltinConstant(Variable("ARGV"))`. Where the idiom `fastq(ARGV[1])` feeds a
  hashed/exported value this diverges: (a) **output hash** — Haskell's `hashOf` shows the use as
  `Lookup 'ARGV' as NGList NGLString`, Rust as `ARGV`, so `auto_comments=[{hash}]` and `collect`'s
  `# Output hash:` differ; (b) **`--export-cwl`** fires `extractARGVUsage'` in Haskell (non-degenerate
  `inputs:`) but never in Rust (empty); (c) **`--export-json`** serialises the node differently
  (Lookup vs BuiltinConstant). No test combines ARGV with hashing/collect/export.

- **samtools `checkUnique` runs at a different phase (same error, earlier timing).** The samtools
  module's `checkUnique` (rejecting `select(..., keep_if=[{unique}])` on a `samtools_sort`ed read
  set) runs in the validate phase in Rust (`validation.rs::validate_select_unique_not_sorted`) vs a
  module transform in Haskell — identical error text, surfaced slightly earlier. (The sibling
  `sortOFormat` transform *is* ported: `transform.rs::sort_oformat` injects `__output_bam=True` so
  `samtools_sort` feeding only a BAM `write` sorts straight to BAM, one `@PG` line, byte-identical.)

- **Out-of-bounds check on an un-bound indexee (`addTemporaries` not ported).** `addIndexChecks`
  *is* now ported (`transform.rs::add_index_checks` + the `__check_index_access` builtin): for
  `array[<constInt>]` where `array` is a variable, a check floats up to just after the array
  assignment and fails **early** with `Index access on line N is invalid.\n …`, byte-identical to
  Haskell. The one remaining gap is `<call>()[<constInt>]` — indexing a call result *directly*
  without binding it to a variable. Haskell catches this because `addTemporaries` runs first and
  lifts the call into a `temp$N` variable, giving the index a `Lookup` target; Rust's
  `extractIndexOne`-equivalent only matches `Lookup` indexees, so this sub-case still errors late.
  Untested and rare (the array is almost always a named variable).

- **Error-handling / leniency on edge inputs (untested):**
  - malformed GFF — an empty attribute field or bare word crashes Haskell (`B.tail ""`), Rust skips
    gracefully (`gff.rs`); Haskell validates the score/phase columns, Rust drops them, so Rust
    accepts GFF lines Haskell rejects.
  - functional-map empty data line — Haskell throws "wrong number of columns", Rust skips.
  - encoding auto-detect — empty quality line or non-multiple-of-4 file crashes/errors in Haskell,
    Rust skips/ignores the tail; missing "Input file is empty" / "cannot be 100% confident" stderr
    warnings.
  - `read[<int>]` single index on a ShortRead — Haskell yields a degenerate `srSlice(a+1,-1)`, Rust
    errors `_evalIndex: invalid operation`.
  - SAM integer field as last token with no trailing tab — Haskell crashes (`B.tail ""`), Rust
    returns `""` (not reachable in well-formed SAM).
  - undefined-variable wording/path differs (type-checker abort `` Could not find variable `x` `` vs
    Haskell's validator `` `"x"` ``).
  - integer literals — Haskell uses arbitrary-precision `Integer`, Rust `i64`, so a huge literal
    panics in Rust (`.parse().unwrap()` in `tokens.rs`).

- **Missing stderr warnings (not output-affecting):** `count()` omits the "both `strand` and `sense`
  … ignored" and "both `norm` and `normalization` …" warnings; assorted version-gated trace/warning
  lines (`select`/`filter`/`allbest`/SAM-merge) are moot or emitted differently at ≥1.5. All stderr,
  not diffed.
