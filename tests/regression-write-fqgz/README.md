# Issue #59

https://github.com/ngless-toolkit/ngless/issues/59


Given
    sample/A.pair.1.fq.gz
    sample/A.pair.2.fq.gz
    sample/B.pair.1.fq.gz
    sample/B.pair.2.fq.gz
    sample/C.single.fq.gz

and

    ngless "0.7"
    import "mocat" version "0.0"

    input = load_mocat_sample(sample)
    write(input, ofile=tmpdir/output.fq)

Produces `output.pair.1.fq`, `output.pair.2.fq`, `output.single.fq` all in the
original compression instead of following the extension provided.

