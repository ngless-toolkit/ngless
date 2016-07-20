=============
Configuration
=============

.. note:: ngless' results do not change because of configuration or command
    line options. **The ngless protocol always has the complete information on
    what is computed**. What configuration options change are details of _how_
    the results are computed such as where to store intermediate files and how
    many CPU cores to use.

Ngless gets its configuration options from the following sources:

1. Defaults/auto-configuration
2. A global configuration file
3. A user configuration file (typically ``$HOME/.config/ngless.conf``)
4. A configuration file present in the current directory`
5. A configuration file specified on the command line
6. Command line options

In case an option is specified more than once, the order above determines
priority: later options take precedence.

Options
-------

``temporary-directory``: where to keep temporary files. By default, this is the
system defined temporary directory (either ``/tmp`` or the value of the
``$TEMPDIR`` environment variable on Unix).

``color``: whether to use color output. Defaults to ``auto`` (i.e., print color
if the output is a terminal), ``no`` (never use color), ``force`` (use color even
if writing to a file or pipe), ``yes`` (synonym of ``force``).

``print-header``: whether to print ngless banner (version info...).

Debug options
~~~~~~~~~~~~~

``keep-temporary-files``: whether to keep temporary files after the end of the programme.

``trace`` (only command line): print a lot of internal information.

