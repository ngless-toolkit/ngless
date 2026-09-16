# NGLess Configuration, Search Paths & Command Line

This reference covers how NGLess is *configured* and how it *finds files*. None of it changes
what a script computes — the NGLess script always has complete information on what is computed.
Configuration and command-line options only change *how* the result is computed (where temporary
files live, how many CPUs are used, what is printed).

The one thing that does affect results and is therefore deliberately **not** configurable is
`map()`'s `block_size_megabases` (low-memory mode): because it is heuristic and can change
results, it must be given in the script.

---

## Search Path Expansion

Certain string arguments support *search path expansion*: a leading `<name>` or `<>` is replaced
with a directory from the configured search path. The arguments that support it are `map()`'s
`fafile=`, `count()`'s `gff_file=` and `functional_map=`, and any external-module argument
declared with `expand_searchpath: true`.

```ngless
map(input, fafile="<>/my-reference.fa")            # unnamed paths only
map(input, fafile="<references>/my-reference.fa")  # paths named "references", plus unnamed ones
```

### Named and unnamed paths

A search path entry is either unnamed (`/opt/ngless-all`) or named (`references=/opt/ngless-refs`).

Given `fafile="<references>/my-reference.fa"`:

| Search path | Result |
|---|---|
| `['references=/opt/ngless-refs']` | `/opt/ngless-refs/my-reference.fa` |
| `['internal=/opt/ngless-internal', 'references=/opt/ngless-refs']` | `/opt/ngless-refs/my-reference.fa` (the `internal` entry does not match) |
| `['internal=/opt/ngless-internal', 'references=/opt/ngless-refs', '/opt/ngless-all']` | both `/opt/ngless-refs/...` and `/opt/ngless-all/...` are tried, in order |

### Rules

1. A path matching `<([^>]*)>` is expanded.
2. The search path list is filtered: an entry is kept if it is unnamed, or if its name matches the
   requested one. `<>` matches nothing, so only unnamed entries are kept.
3. The surviving paths are tried in order; the first that refers to an existing file wins.

### Setting the search path

```sh
ngless script.ngl --search-path "references=/opt/ngless"   # repeatable
```

or in the configuration file (**it is always a list**, even with one element):

```
search-path = ["references=/opt/ngless"]
```

> Search path expansion is powerful enough to defeat NGLess' reproducibility guarantees (the
> script no longer records which reference file was actually used). Used carefully — a stable,
> documented site-wide path — it simplifies file management instead.

---

## Configuration Files

Options are read from the following sources, **later ones overriding earlier ones**:

1. Defaults / auto-configuration
2. A global configuration file
3. A user configuration file (typically `$HOME/.config/ngless.conf`)
4. A configuration file in the current directory
5. Configuration files given with `-c`/`--config-file` (repeatable)
6. Command-line options

The format is simple assignment:

```
temporary-directory = "/local/ngless-temp/"
jobs = "auto"
search-path = ["references=/opt/ngless"]
```

### Options

| Option | Meaning |
|---|---|
| `jobs` | Number of CPUs to use, or `"auto"` (see below) |
| `strict-threads` | If true, never exceed `jobs` threads, even in bursts (see below) |
| `temporary-directory` | Where to keep temporary files (default: system temp / `$TEMPDIR`) |
| `color` | `auto` (default), `no`, `force`, `yes` (synonym of `force`) |
| `print-header` | Whether to print the NGLess banner |
| `user-directory` | User-writable cache for downloads (Linux default: `$HOME/.local/share/ngless/`) |
| `user-data-directory` | User-writable data cache (default: a `data` directory inside `user-directory`) |
| `index-path` | Where mapper indices are stored |
| `global-data-directory` | Global data directory |
| `keep-temporary-files` | Keep temporary files after the run (debugging) |

`trace` is command-line only.

Several of these have no command-line equivalent (`user-directory`, `user-data-directory`,
`global-data-directory`, `print-header`), so a config file is the only way to set them. This is
how the mOTUs integration redirects module and data storage:

```
user-directory = "/your/folder/"
user-data-directory = "/your/folder/"
temporary-directory = "/scratch/your_folder/temp/"
```

### `jobs = "auto"`

With `auto`, NGLess inspects the environment for a CPU count, in particular:

- `OMP_NUM_THREADS`
- `NSLOTS`
- `LSB_DJOB_NUMPROC`
- `SLURM_CPUS_PER_TASK`

If none is found (or none holds a single number), an error is produced. Note these are *not* the
same variables as the `batch` module's (`LSB_JOBINDEX`, `SGE_TASK_ID`, …), though
`SLURM_CPUS_PER_TASK` is used by both.

### `strict-threads`

By default NGLess may briefly exceed `jobs`: it passes the thread count through to an external
mapper such as `bwa` while still using its own threads to process that mapper's output. With
`--strict-threads`, it calls `bwa` with one thread fewer and restricts itself to a single thread,
so even peak usage stays within the limit.

---

## Command-Line Options

**Note:** `ngless --help` currently lists only a subset of these. The authoritative list is
`parse_options` in `src/cli.rs`.

### Running a script

| Option | Meaning |
|---|---|
| `-e`, `--script SCRIPT` | Inline script (no report directory unless forced) |
| `-p`, `--print-last` | Print the value of the last expression to stdout |
| `-n`, `--validate-only` | Validate only; do not run |
| `-j`, `--jobs`, `--threads N` | Thread count |
| `--strict-threads` / `--no-strict-threads` | Strict or soft upper limit on threads |
| `-t`, `--temporary-directory PATH` | Temporary file location |
| `--keep-temporary-files` / `--no-keep-temporary-files` | Keep temp files |
| `-c`, `--config-file PATH` | Configuration file (repeatable) |
| `--search-path PATH` | Reference search directory (repeatable) |
| `--index-path PATH` | Index storage directory |
| `--subsample` | Subsample mode (see SKILL.md — it also renames outputs) |
| `-o`, `--html-report-directory PATH` | Report output directory |
| `--create-report` / `--no-create-report` | Force / disable the HTML run report |
| `-v`, `--verbosity quiet\|normal\|full` | Verbosity |
| `-q`, `--quiet` | Suppress informational output |
| `--trace` / `--no-trace` | Maximum verbosity |
| `--no-header` | Do not print the run header |
| `--color auto\|no\|force\|yes` | Color output |
| `--debug MODE` | Debug output (`ast` dumps the transformed AST) |
| `--experimental-features` | Required by `--export-json` / `--export-cwl` |
| `--export-json FILE` | Export the script as JSON |
| `--export-cwl FILE` | Generate a CWL wrapper for the script |

Long options also accept the `--opt=value` form. Short options bundle and may be joined to their
value: `-nq`, `-j4`, `-nj4`, `-vfull`, `-eprint(1)`, `-pe '...'`.

`ngless --debug-parse FILE` (handled in `lib.rs`, before normal option parsing) dumps the parse
tree for a single file.

### Informational modes

| Option | Meaning |
|---|---|
| `-V`, `--version`, `--version-short`, `--version-debug`, `--date-short` | Version information |
| `--check-install` | Verify that external tools are present |
| `--print-path EXEC` | Print the resolved path of an external tool |
| `-h`, `--help` | Help |

### Data-management modes

These do not run a script:

```sh
ngless --install-reference-data bosTau4     # pre-install a builtin reference
sudo ngless --install-reference-data bosTau4 # ... for all users

ngless --download-demo gut-short            # only 'gut-short' and 'ocean-short' exist
ngless --download-file --download-url URL --local-file PATH

ngless --create-reference-pack \
    --output-name ref.tar.gz \
    --genome-url URL \
    [--gtf-url URL] \
    [--functional-map-url URL]
```

Builtin references are otherwise downloaded on first use and cached in the user directory.
`--subsample` is a convenient way to force all indices and downloads for a pipeline to be
prepared without processing the real data.

### Environment variables

| Variable | Meaning |
|---|---|
| `NGLESS_SAMTOOLS_BIN`, `NGLESS_BWA_BIN`, `NGLESS_MINIMAP2_BIN`, `NGLESS_PRODIGAL_BIN`, `NGLESS_MEGAHIT_BIN` | Override the path to an external tool |
| `NGLESS_DOWNLOAD_BASE_URL` | Override the reference/demo download server |
| `NGLESS_MODULE_DIR` | Set by NGLess for external module commands: the module's directory |
| `NGLESS_NR_CORES` | Set by NGLess for external module commands: the worker thread count |
| `OMP_NUM_THREADS`, `NSLOTS`, `LSB_DJOB_NUMPROC`, `SLURM_CPUS_PER_TASK` | Consulted by `jobs = "auto"` |
| `LSB_JOBINDEX`, `SGE_TASK_ID`, … | Consulted by the `batch` module |
