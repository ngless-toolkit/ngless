=========
Functions
=========

fastq
-----

Argument type: ``String``

Keyword arguments:
- ``type``: symbol, one of ``:auto:`` (default) or ``:solexa:``    
- ``compression``: symbol, one of ``:auto:`` (default), ``:none:``, ``:gzip:``,
  or ``:bzip2:``

Block: ``None``

Returns: ``ReadSet``

This function reads one (or more) FastQ files and performs quality control.

