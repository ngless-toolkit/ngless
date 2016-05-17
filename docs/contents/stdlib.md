# Standard library

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


