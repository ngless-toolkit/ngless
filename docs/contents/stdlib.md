# Standard library

## Parallel module

This module allows you to run several parallel computations. It provides two
functions: `lock1` and `collect`.

`lock1` takes a list and returns a single element. It uses the filesystem to
obtain a lock file so that if multiple processes are running at once, each one
will return a different element. Ngless also marks results as *finished* once
you have run a script to completion.

For example

    ngless "0.0"
    import "parallel" version "0.0"

    samples = ['Sample1', 'Sample2', 'Sample3']
    current = lock1(samples)

Now, current will be one of `'Sample1'`, `'Sample2'`, or `'Sample3'`. You can
use this to find your input data:

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

## Mocat module

    import "mocat" version "0.0"

This is a [MOCAT](http://vm-lux.embl.de/~kultima/MOCAT) compatibility layer to
make it easier to adapt projects from MOCAT to ngless.

### Functions

`load_mocat_sample :: string -> readset` this function takes a directory name
and returns a set of reads by scanning the directory for (compressed) FastQ
files.

`coord_file_to_gtf :: string -> string` this function takes a MOCAT-style
`.coord`, converts it internally to a GTF file and returns it.

Example usage:

    ngless "0.0"
    import "mocat" version "0.0"

    sample = load_mocat_sample('Sample1')
    mapped = map(sampled, fafile='data/catalog.padded.fna')
    write(count(mapped, gff_file=coord_file_to_gtf('data/catalog.padded.coord')),
        ofile='counts.txt')

This module can be combined with the parallel module (see above) to obtain a
very smooth upgrade from MOCAT to ngless.

