---
name: ngless
description: >
  Auto-activate when the user mentions NGLess, ngless, NGS processing,
  metagenomics pipelines with NGLess, writing .ngl scripts, or is working
  in the ngless repository (Rust source at src/). Also activate when
  the user asks about preprocessing FASTQ reads, mapping reads to references,
  counting features, or parallel sample processing in the context of NGLess
  scripting or development.
version: 1.0.0
---

# NGLess: NGS Processing with Less Work

NGLess is a domain-specific language (DSL) for next-generation sequencing (NGS) data processing, with a focus on metagenomics. It has a Pythonesque syntax with Ruby-like blocks, is statically typed with type inference, and emphasizes reproducibility and error checking.

**Current version**: 1.6.0 (released 4 August 2026)
**Language version**: `1.6` — the *only* version this build accepts. Declaring anything else (including the older `"1.5"`) is a hard error.
**Implementation**: Rust (the Haskell implementation was removed in 1.6; the Rust code at the repository root is the sole, supported implementation)
**Repository**: https://github.com/ngless-toolkit/ngless

For configuration files, search path expansion and the full command-line surface, see
[references/configuration-and-paths.md](references/configuration-and-paths.md).

For developer/contributor information (building, source layout, adding functions), see
[references/developer-guide.md](references/developer-guide.md).

---

## Script Structure

Every NGLess script starts with a version declaration, followed by optional imports, then the body:

```ngless
ngless "1.6"
import "parallel" version "1.6"

input = fastq('sample.fq.gz')
input = preprocess(input) using |read|:
    read = substrim(read, min_quality=20)
    if len(read) < 30:
        discard
mapped = map(input, reference='hg19')
counts = count(mapped, features=['seqname'])
write(counts, ofile='output.tsv')
```

**Key syntax rules:**
- Indentation: 4 spaces (no tabs)
- Comments: `#`, `//`, `/* */`
- Symbols: `{symbol_name}` (enumeration-like values for function arguments)
- UPPERCASE variables are constants (single assignment)
- Blocks: `using |var|:` followed by indented block
- Semicolons can separate statements (intended for `-e` inline scripts only)
- `discard` skips the current item in a `preprocess` block
- `continue` continues to the next item
- No user-defined functions (only builtins and module functions)
- Strings: single or double quotes, standard backslash escapes
- `if` / `else` work as in Python (`else` takes its own indented block)
- A function call's result cannot be indexed directly: assign it to a variable first

**Module imports:** built-in modules now track the language version, so import them at `version "1.6"`.
Older version strings (e.g. `"1.1"` for `parallel`) still load — with the *latest* behaviour — but print a
deprecation warning.

---

## Data Types

| Type | Description |
|---|---|
| String | Text, single or double quoted |
| Integer | Decimal or hex (`0x...`) |
| Double | Decimal with `.` separator |
| Bool | `True`/`False` (case-insensitive) |
| Symbol | `{name}` — enumeration-like, used for function options |
| Filename | File path (string subtype) |
| Read (ShortRead) | A single sequencing read |
| ReadSet | Collection of reads |
| MappedRead | A single mapped read |
| MappedReadSet | Collection of mapped reads |
| SequenceSet | A set of sequences (what `assemble()` returns) |
| Counts (CountTable) | A counts table |
| List of X | Homogeneous list: `[1, 2, 3]` |

**Operators:** `+`, `-`, `*`, `<`, `>`, `<=`, `>=`, `==`, `!=`, `</>` (path join), `not`, `len()`
Mixing Integer and Double in an arithmetic expression yields a Double.

**Indexing:** slicing a read — `read[3:]`, `read[:10]`, `read[5:20]`, `read[:]` — and single-element
indexing of a list — `samples[0]`. The indexed thing must be a *variable*: write
`xs = readlines('samples.txt')` then `xs[0]`, not `readlines('samples.txt')[0]`.

---

## Builtin Functions

### Input

| Function | Signature | Description |
|---|---|---|
| `fastq(path)` | String -> ReadSet | Load FastQ file. Args: `encoding={auto}/{33}/{sanger}/{64}/{solexa}`, `interleaved=Bool` |
| `paired(f1, f2)` | String, String -> ReadSet | Load paired-end (the second file is the `second=` argument, usually given positionally). Args: `singles=path`, `encoding=` |
| `load_fastq_directory(dir)` | String -> ReadSet | Auto-discover FASTQ files in directory. Args: `encoding=` |
| `load_sample_list(path)` | String -> [ReadSet] | Load all samples described in a YAML sample list |
| `load_sample_from_yaml(path)` | String -> ReadSet | Load one sample from a YAML file. Args: `sample=` (required) |
| `group([rs1, rs2, ...])` | [ReadSet] -> ReadSet | Combine multiple ReadSets. Args: `name=` |
| `samfile(path)` | String -> MappedReadSet | Load SAM/BAM. Args: `name=`, `headers=` |
| `countfile(path)` | String -> CountTable | Load TSV counts |

#### `load_fastq_directory` file-naming rules

The directory is scanned and files are paired up automatically, so the names must follow the
expected convention:

- `.gz`, `.bz2` and `.xz` are handled transparently and stripped first
- the remaining extension must be `.fq` or `.fastq`
- before that extension, `.1`/`.2`, `_1`/`_2`, or `_F`/`_R` marks a paired-end pair; anything else
  is treated as single-end

A directory containing `S1.pair.1.fq.gz`, `S1.pair.2.fq.gz` and `S1.single.fq.gz` therefore loads as
one sample with both paired-end and single-end data. If your data does not follow these rules, build
a directory of symlinks that does.

#### YAML sample lists

`load_sample_list` / `load_sample_from_yaml` read this format:

```yaml
basedir: /share/data/metagenomes     # optional
samples:
  sample1:
    - paired:
        - data/Sample1a.1.fq.gz
        - data/Sample1a.2.fq.gz
    - paired:
        - data/Sample1b.1.fq.gz
        - data/Sample1b.2.fq.gz
  sample2:
    - paired:
        - data/Sample2.1.fq.gz
        - data/Sample2.2.fq.gz
    - single:
        - data/Sample2.A.fq.gz
```

Each sample maps to a list of entries, each being either `paired:` (two paths) or `single:` (one
path). Relative paths resolve against `basedir` if present, otherwise **against the current working
directory — not the location of the YAML file**.

Use `.name()` to recover the sample name from a loaded ReadSet:

```ngless
input = run_for_all_samples(load_sample_list('list.yaml'))
write(input, ofile='outputs' </> input.name() + '.fq.xz')
```

### Preprocessing

```ngless
input = preprocess(input) using |read|:
    read = read[3:]                              # trim first 3 bases
    read = substrim(read, min_quality=20)         # longest substring >= quality
    read = endstrim(read, min_quality=20)         # trim ends below quality
    read = smoothtrim(read, min_quality=15, window=3)  # sliding window trim
    read = read.n_to_zero_quality()               # set N bases to quality 0
    if read.avg_quality() < 25:
        discard
    if read.fraction_at_least(20) < 0.5:
        discard
    if len(read) < 30:
        discard
```

- `preprocess(readset) using |read|:` — per-read processing block. Args: `keep_singles=Bool`
- `endstrim` args: `min_quality=` (required), `from_ends={both}/{3}/{5}`
- `smoothtrim` args: `min_quality=` (required), `window=`
- `unique(readset)` — deduplicate reads. Args: `max_copies=Int` (default 2)
- `discard_singles(readset)` — remove unpaired reads

### Mapping

```ngless
mapped = map(input, reference='hg19')
mapped = map(input, fafile='ref.fa')
```

Args: `reference=` (builtin or module-provided name), `fafile=` (FASTA path), `mode_all=Bool`,
`mapper=` (`'bwa'` or `'minimap2'`), `block_size_megabases=Int`, `__extra_args=[String]`

`mode_all=True` passes `-a` to bwa (report all alignments). Using minimap2 requires *both*
`import "minimap2" version "1.6"` and `mapper="minimap2"`.

**Low-memory mode**: `block_size_megabases=N` splits a large reference into blocks of roughly `N`
megabases, maps against each in turn and combines the results, so a large catalog can be used on a
machine that could not hold its index. This *does change results* (slightly) — which is exactly why
it is a script argument and not a configuration option: NGLess keeps everything that can affect
results inside the script.

Built-in references (downloaded and cached on first use): `bosTau4`, `ce10`, `canFam3`, `dm5`, `dm6`,
`gg4`, `gg5`, `hg19`, `hg38.p7`, `hg38.p10`, `mm10.p2`, `mm10.p5`, `rn5`, `rn6`, `sacCer3`, `susScr11`.
External modules (`igc`, `om-rgc`, the gut catalogs, ...) contribute further reference names.

External tools are **not** bundled: `samtools`, `bwa`, `minimap2`, `prodigal`, and `megahit` must be on
`$PATH` (or pointed at with `NGLESS_SAMTOOLS_BIN`, `NGLESS_BWA_BIN`, `NGLESS_MINIMAP2_BIN`,
`NGLESS_PRODIGAL_BIN`, `NGLESS_MEGAHIT_BIN`). `ngless --check-install` verifies this.

### Filtering

```ngless
mapped = select(mapped, keep_if=[{mapped}])
mapped = select(mapped, drop_if=[{unmapped}])
mapped = select(mapped, keep_if=[{mapped}, {unique}])
```

Args: `keep_if=[Symbol]`, `drop_if=[Symbol]`, `paired=Bool`
Symbols: `{mapped}`, `{unmapped}`, `{unique}`

By default `select` considers the **insert as a whole** (both mates together); pass `paired=False`
to have each mate judged independently, as if the data were single-end.

### Block-based select with methods

```ngless
filtered = select(mapped) using |mread|:
    mread = mread.filter(min_identity_pc=97, min_match_size=30, max_trim=5)
    mread = mread.allbest()
    if not mread.flag({mapped}):
        discard
    if mread.some_match('contamination_ref'):
        discard
```

MappedRead methods:
- `.filter(min_match_size=, min_identity_pc=, max_trim=, action={drop}/{unmatch}, reverse=Bool)`
- `.allbest()` — keep only the best alignments. "Best" is the *fractional* distance: the `NM` field
  divided by the length of the match. A match with 3 errors over 100 bp therefore beats one with 0
  errors over 90 bp
- `.unique()` — keep the read only if it maps uniquely (otherwise drop all its alignments)
- `.flag({mapped})` / `.flag({unmapped})` — check mapping status
- `.some_match(reference)` — check if mapped to a specific reference
- `.pe_filter()` — keep only reads where both mates mapped

Other methods: `.name()` on a ReadSet (its name), `.to_string()` on Integer/Double.

### Counting

```ngless
counts = count(mapped, features=['seqname'])
counts = count(mapped, features=['gene'], gff_file='annotation.gff')
counts = count(mapped, features=['KEGG_ko'], functional_map='eggnog.tsv')
```

| Argument | Values | Default |
|---|---|---|
| `features` | `['seqname']`, `['gene']`, custom | `['gene']` |
| `subfeatures` | `[String]` (sub-feature columns) | - |
| `gff_file` | path to GFF/GTF | - |
| `functional_map` | path to TSV (col1=seqname, rest=annotations) | - |
| `reference` | name of a module-provided reference with annotations | - |
| `mode` | `{union}`, `{intersection_non_empty}`, `{intersection_strict}` | `{union}` |
| `multiple` | `{all1}`, `{dist1}`, `{1overN}`, `{unique_only}` | `{dist1}` |
| `sense` | `{both}`, `{sense}`, `{antisense}` | `{both}` |
| `normalization` | `{raw}`, `{normed}`, `{scaled}`, `{fpkm}` | `{raw}` |
| `min` | Integer (minimum count threshold) | 0 |
| `include_minus1` | Bool (include unmapped fraction) | True |
| `discard_zeros` | Bool | False |

Exactly **one** annotation source may be given (seqname mode, `gff_file`, `functional_map`, or
`reference`); passing more than one is an error. If the reads were mapped with `map(reference=...)`
against a reference that carries annotations, `count()` can use them with *no* annotation argument
at all. The deprecated `strand` argument was removed in 1.6 — use `sense` (`strand=True` is
`sense={sense}`). `gff_file` and `functional_map` support search path expansion.

#### Choosing `multiple`

- `{unique_only}` — use only uniquely mapped inserts
- `{all1}` — an insert mapping to 4 locations adds 1 to *each*
- `{1overN}` — an insert mapping to 4 locations adds 0.25 to each
- `{dist1}` (default) — distribute multiple mappers in proportion to how the uniquely mapped inserts
  are distributed among those locations

Rule of thumb: `{dist1}` for **gene abundances**, `{all1}` for **functional annotations**. With
`{all1}` the functional totals will exceed the number of reads — that is intended, since one insert
legitimately carries several annotations.

#### Choosing `mode` (how partial overlaps count)

- `{union}` (default) — a read counts for every feature it overlaps
- `{intersection_non_empty}` — a read counts only for features it *exclusively* overlaps, even partially
- `{intersection_strict}` — a read counts only if the whole read overlaps the same feature(s)

#### `normalization`

- `{raw}` (default) — no normalization
- `{normed}` — `{raw}` divided by the feature size
- `{scaled}` — `{normed}` rescaled so the total matches `{raw}` (within rounding)
- `{fpkm}` — fragments per 1000 bp per million fragments

#### GFF or TSV?

Use `functional_map` (TSV) if you can; use `gff_file` only if you must. A TSV annotates each whole
reference sequence with a set of terms — right for gene catalogs. A GFF annotates *regions*, which
you need for mapping against reference genomes or for (meta)transcriptomes, but it is significantly
costlier in both time and memory.

#### `functional_map` TSV format

First column is the sequence name, the rest are annotation columns (this is what
[eggnog-mapper](https://eggnog-mapper.embl.de/) produces):

```
#geneID	feat1	feat2	feat3
G1	a1,a2	b	c
G2	a1|a3		c
```

- The header is one line, optionally starting with `#`. A multi-line header is allowed if *every*
  line starts with `#` — the last such line is the header.
- Multiple values in a cell are separated by `,` or `|`.
- Cells may be empty: for `feat2`, inserts mapped to `G2` count as unmapped.
- **Spaces are not allowed**: `a, b` is the feature `a` and the feature ` b` (with a leading space).

### Assembly & ORF Finding

```ngless
contigs = assemble(input)                    # uses MEGAHIT; returns a SequenceSet
orfs = orf_find(contigs, is_metagenome=True) # uses Prodigal
write(orfs, ofile='orfs.fna')
```

`orf_find` args: `is_metagenome=Bool` (required), `include_fragments=Bool`, `coords_out=path`, `prots_out=path`

### Output

```ngless
write(counts, ofile='output.tsv')
write(counts, ofile='output.csv', format={csv})
write(mapped, ofile='output.bam', format={bam})
write(input, ofile='reads.fq.gz')
write(counts, ofile='output.tsv', auto_comments=[{date}, {script}, {hash}], comment="My analysis")
```

- `format`: `{tsv}`, `{csv}`, `{sam}`, `{bam}`
- Compression auto-detected from extension: `.gz`, `.bz2`, `.xz`, `.zstd`
- `format_flags`: `[{interleaved}]`, `[{always_3_fq_files}]`
- `compress_level=Int`, `verbose=Bool`
- `write()` returns the filename used. The output *directory* is checked before the script runs, even
  when the file name is only computed at run time.

### Utilities

- `print(value)` / `println(value)` — console output (accepts String/Integer/Double)
- `readlines(filename)` — returns `[String]` of non-empty lines
- `read_int(string)` — parse integer. Args: `on_empty_return=default`
- `read_double(string)` — parse double. Args: `on_empty_return=default`
- `qcstats({fastq})` / `qcstats({mapping})` — QC statistics as a CountTable
- `mapstats(mapped)` — mapping statistics (reads, mapped, unique)
- `as_reads(mapped)` — convert MappedReadSet to ReadSet
- `ARGV` — command-line arguments list
- `STDIN` / `STDOUT` — special file descriptors

---

## Modules

### parallel

```ngless
ngless "1.6"
import "parallel" version "1.6"

samples = readlines('samples.txt')
current = run_for_all(samples)
input = paired("data/" + current + ".1.fq.gz", "data/" + current + ".2.fq.gz")
mapped = map(input, reference='hg19')
counts = count(mapped, features=['seqname'])
collect(counts, ofile='all_counts.tsv.gz')
```

- `run_for_all(list)` — process one sample per NGLess invocation (auto-lock, auto-collect). Args: `tag=`
- `run_for_all_samples(readsets)` — same, over a list of ReadSets (e.g. from `load_sample_list`)
- `lock1(list)` — manual locking (more flexible, requires `current=`/`allneeded=` on `collect`)
- `collect(counts, ofile=path)` — aggregate results when all samples are done. Also accepts
  `comment=`/`auto_comments=`
- `set_parallel_tag(tag)` — set the tag used for lock/partial-result directories

Run multiple instances: each picks a different sample via filesystem locks.

**`run_for_all` vs `lock1`**: the real difference is that only **one** `run_for_all` is allowed per
script (it applies to the whole script), while `lock1` may be called multiple times. `lock1` is more
flexible but requires passing `current=` and `allneeded=` to `collect` explicitly:

```ngless
samples = readlines('input.txt')
sample = lock1(samples)
...
collect(counts, current=sample, allneeded=samples, ofile='output.tsv')
```

**Internals worth knowing** (they explain surprising behaviour):

- Lock files live in a subdirectory of `ngless-locks/` named by the **hash of the script**. Any
  change to the script — even a whitespace-only one — starts a fresh directory and forces everything
  to be recomputed. This over-computes rather than risk serving stale results.
- `collect()` hashes both the script and the position within it, so multiple `collect()` calls do
  not clash.
- Lock mtimes are refreshed every 10 minutes; a lock older than one hour is considered stale and
  removed. Because outputs are always written atomically, the worst case from mis-detecting a stale
  lock is wasted computation, never a corrupt or wrong result.

### samtools

```ngless
import "samtools" version "1.6"
sorted = samtools_sort(mapped)                          # by coordinate (default)
sorted = samtools_sort(mapped, by={name})               # by name
regional = samtools_view(mapped, bed_file='regions.bed') # filter by BED
```

### batch

```ngless
import "batch" version "1.6"
```

Exposes no functions; it reads the batch scheduler's environment (`LSB_JOBINDEX`, `SGE_TASK_ID`,
`SLURM_CPUS_PER_TASK`, ...) to set the worker thread count from the job's CPU allotment and to provide
two constants: `JOBINDEX_OR_0` (Integer; `0` when not under a scheduler) and `JOBINDEX_VALID` (Bool).

### minimap2

```ngless
import "minimap2" version "1.6"
```

Exposes no functions; importing it activates the minimap2 mapper for `map()`.

### mocat

- `load_mocat_sample(path)` — load MOCAT-style sample (deprecated: use `load_fastq_directory`)

### External modules

External modules are directories `Modules/<name>.ngm/<version>/module.yaml` (searched in the repository,
the global and the user data directories). They can add references and command-line functions. A plain
`import` is only accepted for *known* modules — `example-cmd`, `gmgc`, `igc`, `om-rgc`, `DogGutCatalog`,
`MouseGutCatalog`, `PigGutCatalog`, `specI`, `motus` — which are auto-downloaded when missing. Any other
module must be brought in with `local import "name" version "..."`.

The legacy built-in `motus`/`soap` modules were removed: a plain `import "motus"` aborts with a guidance
error, but `local import "motus"` (loading the downloaded `motus.ngm` external module) is the supported
way to run motus.

**Version strings**: built-in modules track the NGLess version (`version "1.6"`), but external
modules carry their *own* versions — e.g. `import "igc" version "0.0"`. Do not "fix" an external
module's version to `1.6`.

See `docs/sources/modules.md` for the full `module.yaml` specification.

---

## Installation

```sh
conda install -c bioconda ngless        # recommended; pulls in bwa/samtools/minimap2/megahit/prodigal
```

With [pixi](https://pixi.sh), create a directory containing a manifest named exactly `pixi.toml`
with `ngless = ">=1.6.0,<2"` under `[dependencies]` (channels: `conda-forge` and bioconda), then
`pixi install`. A ready-made copy is `pixi_install_ngless.toml` in the repository.

From source: `git clone https://github.com/ngless-toolkit/ngless && cd ngless && cargo build
--release` produces `target/release/ngless`. A source build does **not** bring the external tools;
put them on `$PATH` and verify with `ngless --check-install`.

---

## Command-Line Usage

```bash
ngless script.ngl                     # run a script
ngless -j 8 script.ngl                # use 8 threads
ngless -n script.ngl                  # validate only (no execution)
ngless --subsample script.ngl         # quick test (discard 99% of data)
ngless -e 'ngless "1.6"; print(ARGV)' # inline script
ngless --trace script.ngl             # maximum verbosity
ngless --create-report script.ngl     # force an HTML QC report
ngless --check-install                # verify external tools are available
```

Key options:
- `-j`, `--jobs`, `--threads N` — thread count
- `-t`, `--temporary-directory PATH` — temp file location
- `--keep-temporary-files` / `--no-keep-temporary-files`
- `-o`, `--html-report-directory PATH` — report output directory
- `--create-report` / `--no-create-report` — force/disable the HTML run report
- `--search-path PATH` — reference search directories (repeatable; the old `--search-dir` was removed)
- `--index-path PATH` — index storage
- `-c`, `--config-file PATH` — configuration file
- `-v`, `--verbosity quiet|normal|full`, `-q/--quiet`, `--trace`, `--no-header`, `--color`
- `-p`, `--print-last` — print the last value computed
- `--strict-threads` / `--no-strict-threads`
- `--print-path EXEC` — print the resolved path of an external tool
- `--export-json=FILE`, `--export-cwl=FILE` — require `--experimental-features`

**HTML run report**: by default a run writes `<script>.output_ngless/` (or the `-o` directory) containing a
single self-contained `index.html` (no network requests, works offline), plus `script.ngl`, `fq.tsv`, and
`mappings.tsv`. Inline scripts (`-e`) do not write a report unless `--create-report`/`-o` is given.

**`--subsample` does two things** beyond throwing away >90% of the data: it rewrites every `write()`
so the output gains a `.subsample` extension (`results.txt` → `results.txt.subsample`), so subsampled
output can never be confused with the real thing; and it still builds every index and downloads every
reference the script needs, which makes it the cheapest way to prepare a pipeline's data ahead of a
real run. Never use it in production.

Short options bundle and may be joined to their values: `-nq`, `-j4`, `-nj4`, `-vfull`, `-pe '...'`.

**`ngless --help` currently lists only a subset of the accepted options.** There are also several
modes that do not run a script at all — `--install-reference-data`, `--download-demo`,
`--download-file`, `--create-reference-pack`. See
[references/configuration-and-paths.md](references/configuration-and-paths.md) for the full list,
plus configuration files and search path expansion.

---

## Common Workflow Patterns

### Removing contaminant / host reads

The blunt form is `select(mapped, drop_if=[{mapped}])`, but the idiom used throughout the NGLess
documentation and tutorials first raises the bar for what counts as a match, so that short spurious
hits do not cause good reads to be thrown away:

```ngless
mapped = map(input, reference='hg19')
mapped = select(mapped) using |mr|:
    mr = mr.filter(min_match_size=45, min_identity_pc=90, action={unmatch})
    if mr.flag({mapped}):
        discard
input = as_reads(mapped)
```

`action={unmatch}` keeps the read but clears its match information, so the following
`flag({mapped})` test only sees matches that passed the identity and length thresholds. Prefer this
over a bare `drop_if=[{mapped}]` when filtering against a host or contaminant genome.

### Metagenomics profiling (single sample)

```ngless
ngless "1.6"
input = paired('sample.1.fq.gz', 'sample.2.fq.gz')
input = preprocess(input) using |read|:
    read = substrim(read, min_quality=25)
    if len(read) < 45:
        discard
mapped = map(input, reference='hg19')
mapped = select(mapped) using |mr|:         # remove human reads
    mr = mr.filter(min_match_size=45, min_identity_pc=90, action={unmatch})
    if mr.flag({mapped}):
        discard
input = as_reads(mapped)
mapped = map(input, fafile='gene_catalog.fna')
counts = count(mapped, features=['KEGG_ko'], functional_map='catalog.map.tsv')
write(counts, ofile='functional_profile.tsv')
```

### Parallel multi-sample processing

```ngless
ngless "1.6"
import "parallel" version "1.6"

current = run_for_all(readlines('samples.txt'))
input = load_fastq_directory("data/" + current)
input = preprocess(input) using |read|:
    read = substrim(read, min_quality=25)
    if len(read) < 45:
        discard
mapped = map(input, fafile='reference.fna')
collect(count(mapped, features=['seqname']), ofile='all_counts.tsv.gz')
```

### Assembly + gene prediction

```ngless
ngless "1.6"
input = fastq('sample.fq.gz')
contigs = assemble(input)
write(contigs, ofile='contigs.fna')
orfs = orf_find(contigs, is_metagenome=True, prots_out='proteins.faa')
write(orfs, ofile='orfs.fna')
mapped = map(input, fafile=contigs)
write(count(mapped, features=['seqname']), ofile='contig_counts.tsv')
```

---

## Common Pitfalls

1. **Indexing a function call's result** is not supported: use `xs = readlines(f)` then `xs[0]`,
   never `readlines(f)[0]`.
2. **Wrong version declaration**: the first non-comment line must be `ngless "1.6"`. `ngless "1.5"` (and
   every other version) is a hard error in this build — there is no compatibility mode.
3. **Old module import versions**: `import "parallel" version "1.1"` still works but warns; use `"1.6"`.
   This applies to *built-in* modules only — external modules keep their own versions
   (`import "igc" version "0.0"`).
4. **Using tabs**: only spaces allowed (4-space indent)
5. **Forgetting `discard`**: in `preprocess` blocks, filtered reads must be explicitly discarded with `discard`
6. **`select` symbols confusion**: `keep_if=[{mapped}]` keeps mapped reads; `drop_if=[{mapped}]` removes them.
   For host/contaminant removal prefer the `filter(..., action={unmatch})` idiom (see Common Workflow
   Patterns) over a bare `drop_if`.
7. **`count` annotation sources**: if features != `['seqname']`, you need exactly one of `gff_file=`,
   `functional_map=`, or `reference=` — giving two or more is an error
8. **`strand=` on `count()`**: removed; use `sense={sense}`
9. **Spaces in a `functional_map` TSV** are part of the feature name: `a, b` means `a` and ` b`
10. **`load_fastq_directory` is picky about names**: `.fq`/`.fastq` (after any `.gz`/`.bz2`/`.xz`),
    paired as `.1`/`.2`, `_1`/`_2`, or `_F`/`_R`. Non-conforming data needs a symlink directory.
11. **Relative paths in a YAML sample list** resolve against `basedir`, or else the current working
    directory — *not* the directory holding the YAML file
12. **Parallel scripts**: must run multiple NGLess processes (one per sample) — the `parallel` module
    coordinates via filesystem locks
13. **`collect` waits for all samples**: output is only written when every sample in the list has been processed
14. **Editing a script invalidates parallel progress**: lock directories are named by the script hash,
    so any edit (whitespace included) forces every sample to be reprocessed
15. **Missing external tools**: bwa/samtools/minimap2/prodigal/megahit are not bundled; install them (e.g.
    via conda/pixi) and check with `ngless --check-install`
16. **The `{hash}` auto-comment value changed in 1.6**: it is an internal, content-addressed identifier and
    is not comparable across releases
