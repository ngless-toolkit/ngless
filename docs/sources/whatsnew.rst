====================
What's New (History)
====================


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
