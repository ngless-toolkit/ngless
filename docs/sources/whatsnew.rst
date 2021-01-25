====================
What's New (History)
====================

Version 1.3.0
-------------

Released *28 January 2021*

User-visible improvements
~~~~~~~~~~~~~~~~~~~~~~~~~

- Adds conversion from string to numbers (int or double) and back
- Better error message if the user attempts to use the non-existent ``<\>``
  operator (suggest ``</>``)
- Validate ``count()`` headers on ``--validate-only``

Internal improvements
~~~~~~~~~~~~~~~~~~~~~

- Use zstd compression for more temporary files

Bugfixes
~~~~~~~~
- Fix cases where sample names contain ``/`` and ``collect()`` (`issue 141
  <https://github.com/ngless-toolkit/ngless/issues/141>`__)


Version 1.2.0
-------------

Released *12 July 2020*.

User-visible improvements
~~~~~~~~~~~~~~~~~~~~~~~~~

- Added function `load_fastq_directory <Functions.html#load_fastq_directory>`__
  to the builtin namespace. This was previously available under the ``mocat``
  module, but it had become much more flexible than the original MOCAT version,
  so it was no longer a descriptive name.
- Better messages in `parallel
  <https://ngless.embl.de/stdlib.html?highlight=lock1#parallel-module>`__
  module when there are no free locks.


Internal improvements
~~~~~~~~~~~~~~~~~~~~~

- Modules can now specify their annotation as a URL that NGLess downloads on a
  "as needed" basis: in version 1.1, only FASTA files were supported.
- Memory consumption of `count() function <Functions.html#count>`__ has been
  improved when using GFF files (*ca.* ⅓ less memory used).
- This one is *hopefully **not** user-visible*: Previously, NGLess would ship
  the Javascript libraries it uses for the HTML viewer and copy them into all
  its outputs. Starting in v1.2.0, the HTML viewer links to the live versions
  online.

Version 1.1.1
-------------

This is a bugfix release and results should not change. In particular, a
sequence reinjection bug was fixed.

Version 1.1.0
-------------

User-visible improvements
~~~~~~~~~~~~~~~~~~~~~~~~~

- Added `discard_singles() function <Functions.html#discard-singles>`__.
- Added ``include_fragments`` option to `orf_find()
  <Functions.html#orf-find>`__.
- The `countfile <https://ngless.embl.de/Functions.html#countfile>`__ now
  reorders its input if it is not ordered. This is necessary for correct usage.
- More flexible loading of ``functional_map`` arguments in `count
  <https://ngless.embl.de/Functions.html#count>`__ to accept multiple comment
  lines at the top of the file as produced by `eggnog-mapper
  <http://eggnog-mapper.embl.de/>`__.
- Added ``sense`` argument to the `count
  <https://ngless.embl.de/Functions.html#count>`__ function, generalizing the
  previous ``strand`` argument (which is deprecated). Whereas before it was
  only possible to consider features either to be present on both strands or
  only on the strand to which they are annotated, now it is also possible to
  consider them present only on the opposite strand (which is necessary for
  some strand-specific protocols as they produce the opposite strand).
- Added ``interleaved`` argument to `fastq
  <https://ngless.embl.de/Functions.html#fastq>`__
- ``load_mocat_sample`` now checks for mismatched paired samples (`#120
  <https://github.com/ngless-toolkit/ngless/issues/120>`__) - Better messages
  when collect call could not finish (following discussion on the `mailing list
  <https://groups.google.com/forum/#!topic/ngless/jIEcC7LVJgI>`__)
- Modules can now specify their resources as a URL that NGLess downloads on a
  "as needed" basis.
- `len <https://ngless.embl.de/Functions.html#len>`__ now works on lists

Internal improvements
~~~~~~~~~~~~~~~~~~~~~

- ZSTD compression is available for output and intermediate files use it for
  reduced temporary space usage (and possibly faster processing).
- Faster check for column headers in ``functional_map`` argument to `count()
  <https://ngless.embl.de/Functions.html#count>`__ function: now it is
  performed *as soon as possible* (including at the top of the script if the
  arguments are literal strings), thus NGLess can fail faster.
- ZSTD compression is available for output and intermediate files use it for
  reduced temporary space usage (and possibly faster processing).
- Faster check for column headers in ``functional_map`` argument to `count()
  <https://ngless.embl.de/Functions.html#count>`__ function: now it is
  performed *as soon as possible* (including at the top of the script if the
  arguments are literal strings), thus NGLess can fail faster.

Version 1.0.1
-------------

This is a bugfix release and results should not change.

Bugfixes
~~~~~~~~

- Fix bug with external modules and multiple fastQ inputs.
- Fix bug with resaving input files where the original file was sometimes
  moved (thus removing it).
- When ``bwa`` or ``samtools`` calls fail, show the user the stdout/stderr from
  these processes (see `#121
  <https://github.com/ngless-toolkit/ngless/issues/121>`__).

Version 1.0
-----------

User-visible improvements
~~~~~~~~~~~~~~~~~~~~~~~~~

- The handling of multiple annotations in `count
  <https://ngless.embl.de/Functions.html#count>`__ (i.e., when the user
  requests multiple ``features`` and/or ``subfeatures``) has changed. The
  previous model caused a few issues (`#63
  <https://github.com/ngless-toolkit/ngless/issues/63>`__, but also mixing with
  `collect() <https://ngless.embl.de/Functions.html#collect>`__. Unfortunately,
  this means that scripts asking for the old behaviour in their version
  declaration are no longer supported if they use multiple features.

Version 0.11
------------

Released March 15 2019 (**0.11.0**) and March 21 2019 (**0.11.1**).

Version 0.11.0 used ZStdandard compression, which was not reliable (the
official haskell zstd wrapper has issues). Thus, it was removed in v0.11.1.
Using v0.11.0 is **not recommended**.

User-visible improvements
~~~~~~~~~~~~~~~~~~~~~~~~~

- Module samtools (version 0.1) now includes `samtools_view`
- Add `--verbose` flag to check-install mode (`ngless --check-install --verbose`)
- Add early checks for input files in more situations (`#33 <https://github.com/ngless-toolkit/ngless/issues/33>`__)
- Support compression in `collect()` output (`#42 <https://github.com/ngless-toolkit/ngless/issues/42>`__)
- Add `smoothtrim() <https://ngless.embl.de/Functions.html#smoothtrim>`__ function

Bugfixes
~~~~~~~~
- Fix bug with `orf_find` & `prots_out` argument
- Fix bug in garbage collection where intermediate files were often left on disk for far longer than necessary.
- Fix CIGAR (`#92 <https://github.com/ngless-toolkit/ngless/issues/92>`__) for select() blocks

Internal improvements
~~~~~~~~~~~~~~~~~~~~~
- Switched to diagrams package for plotting. This should make building easier as cairo was often a complicated dependency.
- Update to LTS-13 (GHC 8.6)
- Update minimap2 version to 2.14
- Call bwa/minimap2 with interleaved fastq files. This avoids calling it twice (which would mean that the indices were read twice).
- Avoid leaving open file descriptors after FastQ encoding detection
- Tar extraction uses much less memory now (`#77 <https://github.com/ngless-toolkit/ngless/issues/77>`__)


Version 0.10.0
--------------

Released Nov 12 2018

Bugfixes
~~~~~~~~
- Fixed bug where header was printed even when STDOUT was used
- Fix to lock1's return value when used with paths (`#68 - reopen <https://github.com/ngless-toolkit/ngless/issues/68>`__)
- Fixed bug where writing interleaved FastQ to STDOUT did not work as expected
- Fix saving fastq sets with --subsample (issue `#85 <https://github.com/ngless-toolkit/ngless/issues/85>`__)
- Fix (hypothetical) case where the two mate files have different FastQ encodings

User-visible improvements
~~~~~~~~~~~~~~~~~~~~~~~~~

- samtools_sort() now accepts by={name} to sort by read name
- Add __extra_megahit_args to assemble() (`issue #86 <https://github.com/ngless-toolkit/ngless/issues/86>`__)
- arg1 in external modules is no longer always treated as a path
- Added expand_searchdir to external modules API (`issue #56 <https://github.com/ngless-toolkit/ngless/issues/56/>`__)
- Support _F/_R suffixes for forward/reverse in load_mocat_sample
- Better error messages when version is mis-specified
- Support `NO_COLOR <https://no-color.org/>`__ standard: when ``NO_COLOR`` is
  present in the environment, print no colours.
- Always check output file writability (`issue #91 <https://github.com/ngless-toolkit/ngless/issues/91>`__)
- ``paired()`` now accepts ``encoding`` argument (it was documented to, but mis-implemented)

Internal improvements
~~~~~~~~~~~~~~~~~~~~~

- NGLess now pre-emptively garbage collects files when they are no longer
  needed (`issue #79 <https://github.com/ngless-toolkit/ngless/issues/79/>`__)

Version 0.9.1
-------------

Released July 17th 2018

- Added `NGLess preprint citation
  <https://www.biorxiv.org/content/early/2018/07/13/367755>`__

Version 0.9
-----------

Released July 12th 2018

User-visible improvements
~~~~~~~~~~~~~~~~~~~~~~~~~

- Added ``allbest()`` method to MappedRead.
- NGLess will issue a warning before overwriting an existing file.
- Output directory contains PNG files with basic QC stats
- Added modules for gut gene catalogs of `mouse <https://www.nature.com/articles/nbt.3353>`__, `pig <https://www.nature.com/articles/nmicrobiol2016161>`__, and `dog <https://microbiomejournal.biomedcentral.com/articles/10.1186/s40168-018-0450-3>`__
- Updated the `integrated gene catalog <https://www.nature.com/articles/nbt.2942>`__

Internal improvements
~~~~~~~~~~~~~~~~~~~~~

- All lock files now are continuously "touched" (i.e., their modification time
  is updated every 10 minutes). This makes it easier to discover stale lock
  files.
- The automated downloading of builtin references now uses versioned URLs, so
  that, in the future, we can change them without breaking backwards
  compatibility.

Version 0.8.1
-------------

Released June 5th 2018

This is a minor release and upgrading is recommended.

Bugfixes
~~~~~~~~

- Fix for systems with non-working locale installations
- Much faster `collect <Functions.html#count>`__ calls
- Fixed `lock1
  <https://ngless.embl.de/stdlib.html?highlight=lock1#parallel-module>`__ when
  used with full paths (see `issue #68 <https://github.com/ngless-toolkit/ngless/issues/68>`__)
- Fix expansion of searchpath with external modules (see `issue #56
  <https://github.com/ngless-toolkit/ngless/issues/56>`__)

Version 0.8
-----------

Released May 6th 2018

Incompatible changes
~~~~~~~~~~~~~~~~~~~~

- Added an extra field to the FastQ statistics, with the fraction of basepairs
  that are not ATCG. This means that uses of `qcstats
  <Functions.hml#qcstats>`__ must use an up-to-date version declaration.

- In certain cases (see below), the output of count when using a GFF will change.

User-visible improvements
~~~~~~~~~~~~~~~~~~~~~~~~~

- Better handling of multiple features in a GFF. For example, using a GFF
  containing "gene_name=nameA,nameB" would result in::

      nameA,nameB    1

    Now the same results in::

      nameA          1
      nameB          1

  This follows after `https://git.io/vpagq <https://git.io/vpagq>`__ and the
  case of *Parent=AF2312,AB2812,abc-3*

- Support for `minimap2 <https://github.com/lh3/minimap2>`__ as alternative
  mapper. Import the ``minimap2`` module and specify the ``mapper`` when
  calling `map <Functions.html#map>`__. For example::

    ngless '0.8'
    import "minimap2" version "1.0"

    input = paired('sample.1.fq', 'sample.2.fq', singles='sample.singles.fq')
    mapped = map(input, fafile='ref.fna', mapper='minimap2')
    write(mapped, ofile='output.sam')

- Added the ``</>`` operator. This can be used to concatenate filepaths. ``p0
  </> p1`` is short for ``p0 + "/" + p1`` (except that it avoids double forward
  slashes).

- Fixed a bug in `select <Functions.html#select>`__ where in some edge cases,
  the sequence would be incorrectly omitted from the result. Given that this is
  a rare case, if a version prior to 0.8 is specified in the version header,
  the old behaviour is emulated.

- Added bzip2 support to `write <Functions.html#write>`__.

- Added reference argument to `count <Functions.html#count>`__.

Bug fixes
~~~~~~~~~

- Fix writing multiple compressed Fastq outputs.

- Fix corner case in `select <Functions.html#select>`__. Previously, it was
  possible that some sequences were wrongly removed from the output.

Internal improvements
~~~~~~~~~~~~~~~~~~~~~

- Faster `collect() <Functions.html#collect>`__
- Faster FastQ processing
- Updated to bwa 0.7.17
- External modules now call their init functions with a lock
- Updated library collection to LTS-11.7

Version 0.7.1
-------------

Released Mar 17 2018

Improves memory usage in ``count()`` and the use the ``when-true`` flag in
external modules.

Version 0.7
-----------

Released Mar 7 2018

New functionality in NGLess language
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


- Added `max_trim <methods.html>`__ argument to ``filter`` method of
  ``MappedReadSet``.
- Support saving compressed SAM files
- Support for saving interleaved FastQ files
- Compute number Basepairs in FastQ stats
- Add ``headers`` argument to `samfile function <Functions.html#samfile>`__

Bug fixes
~~~~~~~~~

- Fix ``count``'s mode ``{intersection_strict}`` to no longer behave as ``{union}``
- Fix ``as_reads()`` for single-end reads
- Fix ``select()`` corner case

In addition, this release also improves both speed and memory usage.


Version 0.6
-----------

Released Nov 29 2017

Behavioural changes
~~~~~~~~~~~~~~~~~~~


- Changed ``include_m1`` default in `count() <Functions.html#count>`__ function
  to True

New functionality in NGLess language
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Added `orf_find <Functions.html#orf_find>`__ function (implemented through
  Prodigal) for open reading frame (ORF) predition

- Add `qcstats() <Functions.html#qcstats>`__ function to retrieve the computed
  QC stats.

- Added reference alias for a more human readable name
- Updated builtin referenced to include latest releases of assemblies

New functionality in NGLess tools
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Add --index-path functionality to define where to write indices.

- Allow `citations` as key in external modules (generally better citations
  information)

- Use multiple threads in SAM->BAM conversion

- Better error checking/script validation

Bug fixes
~~~~~~~~~

- Output preprocessed FQ statistics (had been erroneously removed)
- Fix --strict-threads command-line option spelling
- Version embedded megahit binary
- Fixed inconsistency between reference identifiers and underlying files



Version 0.5.1
-------------

Released Nov 2 2017

Fixed some build issues

Version 0.5
-----------

Released Nov 1 2017

First release supporting all basic functionality.
