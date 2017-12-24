====================
What's New (History)
====================


After version 0.6 (unreleased)
------------------------------

- Added `max_trim <methods.html>`__ argument to ``filter`` method of
  ``MappedReadSet``.

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
