=============
Configuration
=============

.. note:: NGLess' results do not change because of configuration or command
    line options. **The NGLess script always has complete information on what
    is computed**. What configuration options change are details of *how* the
    results are computed such as where to store intermediate files and how many
    CPU cores to use.

Ngless gets its configuration options from the following sources:

1. Defaults/auto-configuration
2. A global configuration file
3. A user configuration file (typically ``$HOME/.config/ngless.conf``)
4. A configuration file present in the current directory`
5. A configuration file specified on the command line
6. Command line options

In case an option is specified more than once, the order above determines
priority: later options take precedence.

Configuration file format
-------------------------

NGLess configuration files are text files using assignment syntax. Here is a
simple example, setting the temporary directory and enabling auto-detection of
the number of threads::

    temporary-directory = "/local/ngless-temp/"
    jobs = "auto"


Options
-------

``jobs``: number of CPUs to use. You can use the keyword ``auto`` to attempt
auto-detection (see below).

``strict-threads``: by default, NGLess will, in certain conditions, use more
CPUs than specified by the ``jobs`` argument (in bursts of activity). This
happens, for example, when it calls an external short-read-mapper (such as `bwa
<http://bio-bwa.sourceforge.net/bwa.shtml>`__). By default, it will pass the
threads argument through to ``bwa``. However, it will still be processing
``bwa``'s output using its own threads. This will results in small bursts of
activity where the CPU usage is above ``jobs``. If you specify
``--strict-threads``, however, then this behavior is curtailed and it will
never use more threads than specified (in particular, it will call ``bwa``
using one thread fewer than specified, while restricting itself to a single
thread, thus even peak usage is at most the number of specified threads).

``temporary-directory``: where to keep temporary files. By default, this is the
system defined temporary directory (either ``/tmp`` or the value of the
``$TEMPDIR`` environment variable on Unix).

``color``: whether to use color output. Defaults to ``auto`` (i.e., print color
if the output is a terminal), ``no`` (never use color), ``force`` (use color even
if writing to a file or pipe), ``yes`` (synonym of ``force``).

``print-header``: whether to print ngless banner (version info...).

``user-directory``: user writable directory to cache downloads (default is
system dependent, on Linux, typically it is ``$HOME/.local/share/ngless/``).

``user-data-directory``: user writable directory to cache data (default is a
``data`` directory inside the ``user-directory`` [see above]).

``index-path``: user writable directory to store indices and similar data.

``global-data-directory``: global data directory.

Debug options
~~~~~~~~~~~~~

``keep-temporary-files``: whether to keep temporary files after the end of the programme.

``trace`` (only command line): print a lot of internal information.

Auto-detection of the number of CPUs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the option ``auto`` is passed as the number of jobs (either on the command
line or in the configuration file), ngless will inspect the environment looking
for a small set of clues as to how many CPUs to use. In particular, it will
make use of these variables:

- ``OMP_NUM_THREADS``
- ``NSLOTS``
- ``LSB_DJOB_NUMPROC``
- ``SLURM_CPUS_PER_TASK``

If none are found (or they do not contain a single number), an error is produced.

