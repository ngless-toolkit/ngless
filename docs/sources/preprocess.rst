========================
Preprocessing FastQ Data
========================

Preprocessing FastQ files consists of quality trimming and filtering of reads
as well as (possible) elimination of reads which match some reference which is
not of interest.

Quality-based filtering
-----------------------

Filtering reads based on quality is performed with the ``preprocess`` function,
which takes a block of code. This block of code will be executed for each read.
For example::

    ngless "0.8"

    input = fastq('input.fq.gz')

    input = preprocess(input) using |r|:
        r = substrim(r, min_quality=20)
        if len(r) < 45:
            discard

If it helps you, you can think of the ``preprocess`` block as a ``foreach``
loop, with the special keyword ``discard`` that removes the read from the
collection. Note that the name ``r`` is just a variable name, which you choose
using the ``|r|`` syntax.

Within the preprocess block, you can modify the read in several ways:

- you can trim it with the indexing operator: ``r[trim5:]`` or ``r[:-trim3]``

- you can call ``substrim``, ``endstrim`` or ``smoothtrim`` to trim the read
  based on quality scores. ``substrim`` finds the longest substring such that
  all bases are above a minimum quality (hence the name, which phonetically
  combines substring and trim). ``endstrim`` chops bases off the ends and
  ``smoothtrim`` averages quality scores using a sliding window before applying
  ``substrim``.

- you can test for the length of the sequence (before or after trimming). For
  this, you use the ``len`` function (see example above).

- you can test for the average quality score (using the ``avg_quality()``
  method).

You can combine these in different ways. For example, the behaviour of the
`fastx quality trimmer <http://hannonlab.cshl.edu/fastx_toolkit/>`__ can be
recreated as::

    preprocess(input) using |r|:
        r = endstrim(r, min_quality=20)
        if r.fraction_at_least(20) < 0.5:
            discard
        if len(r) < 45:
            discard

Handling paired end reads
~~~~~~~~~~~~~~~~~~~~~~~~~

When your input is paired-end, the preprocess call above will handle each mate
independently. Three things can happen:

1. both mates are discarded,
2. both mates are kept (i.e., not discarded),
3. one mate is kept, the other discarded.

The only question is what to do in the third case. By default, the
``preprocess`` call keep the mate turning the read into an unpaired read (a
single), but you can change that behaviour by setting the ``keep_singles``
argument to ``False``::

    preprocess(input, keep_singles=False) using |r|:
        r = substrim(r, min_quality=20)
        if len(r) < 45:
            discard

Now, the output will consist of only paired-end reads.

Filtering reads matching a reference
------------------------------------

It is often also a good idea to match reads against some possible contaminant
database. For example, when studying the host associated microbiome, you will
often want to remove reads matching the host. It is almost always a good to at
least check for human contamination (during lab handling).

For this, you map the reads against the human genome::

    mapped_hg19 = map(input, reference='hg19')

Now, ``mapped_hg19`` is a set of mapped reads. Mapped reads are reads, their
qualities, plus additional information of how they matched. Mapped read sets
are the internal ngless representation of SAM files.

To filter the set, we will ``select``. Like ``preprocess``, ``select`` also
uses a block for the user to specify the logic::

    mapped_hg19 = select(mapped_hg19) using |mr|:
        mr = mr.filter(min_match_size=45, min_identity_pc=90, action={unmatch})
        if mr.flag({mapped}):
            discard

We first set a minimum match size and identity percentage to avoid spurious
hits. **We keep the reads** but **unmatch** them (i.e., we clear any
information related to a match). Then, we discard any reads that match by
checking the flag ``{mapped}``.

Finally, we convert the mapped reads back to simple reads using the
``as_reads`` function (this discards the matching information)::

    input = as_reads(mapped_hg19)

Now, ``input`` can be passed to the next step in the pipeline.
