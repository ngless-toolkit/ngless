# YAML based sample lists

**Since NGLess 1.5**

## Specifying a list of samples in a YAML file:

You can specify a list of samples in YAML format.

    basedir: /share/data/metagenomes
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
            - data/Sample2.extra.fq.gz

The format is the following

- `basedir` *(optional)*: if specified, all relative paths are relative to this directory. Otherwise, paths are relative to the current directory where NGLess is executing (**not** where the YAML file is located)
- `samples`: a dictionary mapping a sample name to a list of files

## Using the YAML format in NGLess

You can load a sample list with the `load_sample_list` function:

    ngless "1.5"
    samples = load_sample_list('list.yaml')
    input = samples[0]

    input = preprocess(input) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard
    ...


It can also be used with the [parallel module](stdlib.html) module's
`run_for_all_samples` function. For example:

    ngless "1.5"
    import "parallel" version "1.1"
    input = run_for_all_samples(load_sample_list('list.yaml'))

    input = preprocess(input) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard
    
    write(input, ofile='outputs' </> input.name() + '.fq.xz')
    ...

Note how we used the `.name()` method in the readset object to get the name of
the selected sample.

## Loading a single sample from an YAML file

The function `load_sample_from_yaml` (which takes a YAML file and a mandatory
`sample` argument) will return a single sample (identified by the `sample`
argument).

    ngless "1.5"
    input = load_sample_from_yaml('list.yaml', sample='sample-id')
    ...

