The `load_fastq_directory` function
-----------------------------------

The `load_fastq_directory` function is one of the main ways to get data _into_
NGLess. It takes the name of a directory

```bash
$ find sample1
sample1/SRR8053346.pair.1.fq.gz
sample1/SRR8053346.pair.2.fq.gz
sample1/SRR8053346.single.fq.gz
sample1/SRR8053355.pair.1.fq.bz2
sample1/SRR8053355.pair.2.fq.bz2
```

This will return a sample that contains _both paired-end **and** single-end data_:

1. The paired-end dataset `sample1/SRR8053346.pair.1.fq.gz` - `sample1/SRR8053346.pair.2.fq.gz`
2. The paired-end dataset `sample1/SRR8053355.pair.1.fq.bz2` - `sample1/SRR8053355.pair.2.fq.bz2`
3. The single-end dataset `sample1/SRR8053346.single.fq.gz`

Currently (as of version `1.4`), `NGLess` supports the following

- Extensions `.gz` and `.bz2` are handled transparently
- The extension (prior to the compression extension) must be either `.fq` or `.fastq`
- Before the extension, one of `.1`/`.2` or `_1`/`_2` or `_F`/`_R` denotes the paired-end matching

If your data does not conform to these rules, we recommend that you use
_symlinks_ to build a directory that does conform to it.

