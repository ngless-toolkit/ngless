# NGLess: NGS Processing with Less Work

This is a domain-specific language for NGS (next-generation sequencing data)
processing.

**Note**: This is *pre-release* software, currently available as a preview
only. Please [get in touch](mailto:coelho@embl.de) if you want to use it in
your work.

## Example

    ngless "0.0"
    input = fastq(['ctrl1.fq','ctrl2.fq','stim1.fq','stim2.fq'])
    preprocess(input) using |read|:
        read = read[5:]
        read = substrim(read, min_quality=26)
        if len(read) < 31:
            discard

    mapped = map(input,
                    reference='hg19')
    annotated = annotate(mapped,
                    features=[{gene}])
    write(count(annotated, count={gene}),
            ofile='gene_counts.csv',
            format={csv})

## Dependencies

Cabal version 1.18 or higher is necessary.

Running `make` should build a sandbox and download/install all dependencies
into it.

## More information

- [Full documentation](http://ngless.readthedocs.org/en/latest/)
- [Frequently Asked Questions (FAQ)](http://ngless.readthedocs.org/en/latest/faq.html)
- [ngless webpage](http://luispedro.github.io/ngless/)

## Authors

- [Luis Pedro Coelho](http://luispedro.org) (email: [coelho@embl.de](mailto:coelho@embl.de)) (on twitter: [@luispedrocoelho](https://twitter.com/luispedrocoelho))
- Paulo Monteiro
- [Ana Teresa Freitas](http://web.tecnico.ulisboa.pt/ana.freitas/)

