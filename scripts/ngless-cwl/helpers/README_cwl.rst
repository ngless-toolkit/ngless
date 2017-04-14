In order to generate .cwl files from the python scripts I used::

    https://github.com/erasche/argparse2tool

Installed on a conda environment with python3.5.

Run::

    argparse2tool_check_path

and follow instructions to setup the environment to use the tool.

Once setup simply run::

    ./make_cwl.sh

and a file called ``myscript.cwl`` should now exist in the current directory.

..note ::
    This script is used to create a template .cwl file from existing scripts.
    The final result will be incomplete. You'll need to manually adjust 'output:' and 'type: File' for 'inputs:'
    The '--debug' option is also left in as it may prove useful for debugging.
