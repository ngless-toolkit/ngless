# Standard library

## Parallel module

This module allows you to run several parallel computations.

**Important**: when you use this module, you will often need to _run the NGLess
command multiple times_ (one for each sample). These can be run in parallel
(and even on different compute nodes on an HPC cluster).

The module provides two functions: `lock1` and `collect`.

`lock1 :: [string] -> string` takes a list of strings and returns a single
element. It uses the filesystem to obtain a lock file so that if multiple
processes are running at once, each one will return a different element. NGLess
also marks results as *finished* once you have run a script to completion.

The intended usage is that you simply run as many processes as inputs that you
have and ngless will figure everything out.

For example

    ngless "1.0"
    import "parallel" version "1.0"

    samples = ['Sample1', 'Sample2', 'Sample3']
    current = lock1(samples)

Now, when you run this script, `current` will be assigned to one of
`'Sample1'`, `'Sample2'`, or `'Sample3'`. You can use this to find your input
data:

    input = paired("data/" + current + ".1.fq.gz", "data/" + current + ".2.fq.gz")

Often, it's a good idea to combine `lock1` with `readlines` (a function which
returns the contents of all the non-empty lines in a file as a list of
strings):

    samples = readlines('samples.txt')
    current = lock1(samples)
    input = paired("data/" + current + ".1.fq.gz", "data/" + current + ".2.fq.gz")

You now use `input` as in any other ngless script:

    mapped = map(input, reference='hg19')
    write(input, ofile='outputs/'+current+ '.bam')
    counts = count(mapped)
    write(counts, ofile='outputs/'+current+ '.txt')

This will result in both BAM files and counts being written to the `outputs/`
directory. The module also adds the `collect` function which can paste all the
counts together into a single table, for convenience:

    collect(
        counts,
        current=current,
        allneeded=samples,
        ofile='outputs/counts.txt.gz')

Now, only when all the samples in the `allneeded` argument have been processed,
does ngless collect all the results into a single table.

#### Full "parallel" example

    ngless "1.0"
    import "parallel" version "1.0"

    sample = lock1(readlines('input.txt'))
    input = fastq(sample)
    mapped = map(input, reference='hg19')
    collect(count(mapped, features=['seqname']),
        current=sample,
        allneeded=readlines('input.txt'),
        ofile='output.tsv')

Now, you can run multiple `ngless` jobs in parallel and each will work on a
different line of `input.txt`.

### Parallel internals

Normally this should be invisible to you, but if you are curious or want to
debug an issue, here are the gory details:

The function `lock1()` will create a lock file in a sub-directory of
`ngless-locks`. This directory will be named by the hash value of the script.
Thus, any change to the script will force all data to be recomputed. This can
lead to over-computation but it ensures that you will always have the most up
to date results (ngless' first priority is correctness, performance is
important, but not at the risk of correctness). Similarly, `collect()` will use
hashed values which encode both the script and the position within the script
(so that if you have more than one `collect()` call, they will not clash).

Lock files have their modification times updated once every 10 minutes while
NGLess is running. This allows the programme to easily identify stale files.
The software is very conservative, but any lock file with a modification time
older than one hour is considered stale and removed. Note that because NGLess
will write always create its outputs atomically, the worse that can happen from
mis-identifying a stale lock (for example, you had a compute node which lost
network connectivity, but it comes back online after an hour and resumes
processing) is that extra computation is wasted, **the processes will never
interfere in a way that you get erroneous results**.

## Samtools module

This module exposes two samtools functionalities: sorting (`samtools_sort`) and
selecting reads in regions of interest (`samtools_view`).

    ngless '1.0'
    import "samtools" version "1.0"
    input = samfile('input.bam')
    sam_regions = samtools_view(input, bed_file="interesting_regions.bed")
    write(sam_regions, ofile='interesting.sam')

`samtools_view :: mappedreadset -> mappedreadset` returns a subset of the
mapped reads that overlap with the regions specified in the BED file.

    ngless '1.0'
    import "samtools" version "1.0"
    to_sort = samfile('input.bam')
    sorted = samtools_sort(to_sort)
    name_sorted = samtools_sort(to_sort, by={name})
    write(sorted, ofile='input.sorted.bam')
    write(name_sorted, ofile='input.name_sorted.bam')

`samtools_sort :: mappedreadset -> mappedreadset` returns a sorted version of
the dataset.

Internally, both function call ngless' version of samtools while respecting
your settings for the use of threads and temporary disk space. When combined
with other functionality, ngless can also often stream data into/from samtools
instead of relying on intermediate files (these optimizations should not change
the visible behaviour, only make the computation faster).

## Mocat module

    import "mocat" version "0.6"

This is a [MOCAT](http://vm-lux.embl.de/~kultima/MOCAT) compatibility layer to
make it easier to adapt projects from MOCAT to ngless.

### Functions

`load_mocat_sample :: string -> readset` this function takes a directory name
and returns a set of reads by scanning the directory for (compressed) FastQ
files. This is slightly more flexible than MOCAT2 in terms of the patterns in
matches. In particular, the following extensions are accepted:

- `fq`
- `fq.gz`
- `fq.bz2`
- `fastq`
- `fastq.gz`
- `fastq.bz2`

Paired-end reads are assumed to be split into two files, with matching names
with `.1`/`.2` appended. `_1`/`_2` as is used by the European Nucleotide
Archive (ENA) is also accepted.

If paired-end reads have been pre-filtered, an unpaired/single file is often available.
`load_mocat_sample` recognizes the suffix `single`. In the following example,
all three files are read as one group:

    sample
    ├── sample.pair.1.fq.gz
    ├── sample.pair.2.fq.gz
    └── sample.single.fq.gz


`coord_file_to_gtf :: string -> string` this function takes a MOCAT-style
`.coord`, converts it internally to a GTF file and returns it.

Example usage:

    ngless "0.6"
    import "mocat" version "0.6"

    sample = load_mocat_sample('Sample1')
    mapped = map(sampled, fafile='data/catalog.padded.fna')
    write(count(mapped, gff_file=coord_file_to_gtf('data/catalog.padded.coord')),
        ofile='counts.txt')

This module can be combined with the parallel module (see above) to obtain a
very smooth upgrade from MOCAT to ngless.

