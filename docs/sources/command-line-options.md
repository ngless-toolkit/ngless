# Command line options

Running `ngless --help` will show you all the command line options. Here we
describe the most important ones.

Most of the command line options can be set in a configuration file, which
defaults to `~/.config/ngless.conf`, but you can set this explicitly:

  --config-file ARG        Configuration files to parse

The [configuraton](configuration.html) section of the manual has more
information on which options can be set in the configuration file. Whenever an
option is set both in the config file and on the command line, then the command
line will take prioriry.

## Using multiple threads

The main option is called `-j` and sets the number of threads.

  -j,--jobs,--threads ARG  Nr of threads to use

Using `--strict-threads/--no-strict-threads` controls whether this is a strict
or soft upper limit.

  --strict-threads         strictly respect the --threads option (by default,
                           NGLess will, occasionally, use more threads than
                           specified)
  --no-strict-threads      opposite of --strict-threads


## Paths

NGLess can generate large temporary files. By default it uses the system's
temporary directory, but it is often a good idea to set it to a path with a lot
of free disk space:

  -t,--temporary-directory ARG
                           Directory where to store temporary files


  --search-path ARG        Reference search directories (replace <references> in
                           script)
  --index-path ARG         Index path (directory where indices are stored)


## Debugging

A few options are useful for debugging:

  -n,--validate-only       Only validate input, do not run script
  --subsample              Subsample mode: quickly test a pipeline by discarding
                           99% of the input
  --trace                  Set highest verbosity mode
  --no-trace               opposite of --trace
  --keep-temporary-files   Whether to keep temporary files (default is delete
                           them)
  --no-keep-temporary-files
                           opposite of --keep-temporary-files


## QC Reporting

  --create-report          create the report directory
  --no-create-report       opposite of --create-report
  -o,--html-report-directory ARG
                           name of output directory

