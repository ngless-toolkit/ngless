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
2. The global configuration file, ``/etc/ngless.conf``
3. The user configuration files, ``$HOME/.ngless.conf`` and then
   ``$HOME/.config/ngless.conf``
4. Configuration files specified on the command line (with ``-c`` or
   ``--config-file``, which can be repeated)
5. Command line options

All configuration files are optional, except those given on the command line.
In case an option is specified more than once, the order above determines
priority: later options take precedence.

.. versionchanged:: 1.6.2
    Previously, ``/etc/ngless.conf`` was read last and so overrode the user
    configuration files.

Configuration file format
-------------------------

NGLess configuration files are text files using assignment syntax. Here is a
simple example, setting the temporary directory and the search path::

    temporary-directory = "/local/ngless-temp/"
    search-path = ["references=/share/ngless-references"]


Options
-------

The number of threads cannot be set in the configuration file. Use the ``-j``
(or ``--jobs``/``--threads``) command line option (see below).

``strict-threads``: by default, NGLess will, in certain conditions, use more
CPUs than specified with ``-j`` (in bursts of activity). This
happens, for example, when it calls an external short-read-mapper (such as `bwa
<https://bio-bwa.sourceforge.net/bwa.shtml>`__). By default, it will pass the
threads argument through to ``bwa``. However, it will still be processing
``bwa``'s output using its own threads. This will results in small bursts of
activity where the CPU usage is above the requested number of threads. If you specify
``--strict-threads``, however, then this behavior is curtailed and it will
never use more threads than specified (in particular, it will call ``bwa``
using one thread fewer than specified, while restricting itself to a single
thread, thus even peak usage is at most the number of specified threads).

``temporary-directory``: where to keep temporary files. By default, this is the
system defined temporary directory (the value of the ``$TMPDIR`` environment
variable or, if it is not set, ``/tmp``).

``color``: whether to use color output. Defaults to ``auto`` (i.e., print color
if the output is a terminal), ``no`` (never use color), ``force`` (use color even
if writing to a file or pipe), ``yes`` (synonym of ``force``).

``print-header``: whether to print ngless banner (version info...).

``user-directory``: user writable directory to cache downloads (default is
system dependent, on Linux, typically it is ``$HOME/.local/share/ngless/``).

``user-data-directory``: user writable directory to cache data (default is a
``data`` directory inside the ``user-directory`` [see above]).

``index-path``: user writable directory to store indices and similar data.

``global-data-directory``: global data directory (default:
``<prefix>/share/ngless/data``, where ``<prefix>`` is the directory containing
``bin/ngless``). Reference data is installed here if it is writable (see
`Organisms <Organisms.html>`__).

``search-path``: the `search path <searchpath.html>`__ (a list of strings).

``create-report``: whether to write the HTML report directory (default: true).

``download-url``: base URL from which reference data, modules, and demos are
downloaded (default: ``https://ngless-resources.big-data-biology.org/``). The
``NGLESS_DOWNLOAD_BASE_URL`` environment variable overrides it.

Debug options
~~~~~~~~~~~~~

``keep-temporary-files``: whether to keep temporary files after the end of the programme.

``trace`` (only command line): print a lot of internal information.

Number of CPUs
~~~~~~~~~~~~~~

The number of threads is set on the command line with ``-j`` (or ``--jobs`` or
``--threads``). Passing ``auto`` (``-j auto``) uses the number of CPUs available
to the process.

When the `batch module <stdlib.html#batch-module>`__ is imported, the number of
threads is instead taken from the CPU allocation advertised by the job
scheduler, using the first of these variables that is set to a number:

- ``OMP_NUM_THREADS``
- ``NSLOTS``
- ``LSB_DJOB_NUMPROC``
- ``SLURM_CPUS_PER_TASK``
