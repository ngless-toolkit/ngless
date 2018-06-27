# Command Line Wrappers

Some of the functionality of NGLess can also be accessed using traditional
command-line scripts. These are written in Python and can be installed using
Python package management tools:

    pip install NGLessPy

All of the wrappers can install NGLess if passed the `--auto-install` flag.

All of these wrappers also have [Common Workflow
Language](http://www.commonwl.org/) so that they can be used in larger
pipelines.

## ngless-install.py

_This is only supported on Linux_

Installs NGLess either for a single user (`$HOME/.local/bin/ngless`) or
globally (`/usr/local/bin'`). All the other tools in this package can also
install NGLess automatically.

    usage: ngless-install.py [-h] [-f] [-t TARGET] [-m {user,global}] [--verbose]

    optional arguments:
      -h, --help            show this help message and exit
      -f, --force           Install NGLess even if it is already found
      -t TARGET, --target TARGET
                            Output file/path for results
      -m {user,global}, --mode {user,global}
                            Global or user install
      --verbose             Verbose mode


## ngless-count.py

This is the equivalent of calling the [count function](Functions.html#count)
from within NGLess:

    usage: ngless-count.py [-h] -i INPUT -o OUTPUT [-f FEATURES]
                           [-m {dist1,all1,1overN,unique_only}] [--auto-install]
                           [--debug]

    optional arguments:
      -h, --help            show this help message and exit
      -i INPUT, --input INPUT
                            SAM/BAM/CRAM file to count reads on
      -o OUTPUT, --output OUTPUT
                            Output file/path for results
      -f FEATURES, --features FEATURES
                            Feature to count
      -m {dist1,all1,1overN,unique_only}, --multiple {dist1,all1,1overN,unique_only}
                            How to handle multiple mappers
      --auto-install        Install NGLess if not found in PATH
      --debug               Prints the payload before submitting to ngless


## ngless-map.py

This is the equivalent of calling the [map function](Functions.html#map)
from within NGLess.

    usage: ngless-map.py [-h] -i INPUT [-i2 INPUT_REVERSE] [-s INPUT_SINGLES] -o
                         OUTPUT [--auto-install] [--debug]
                         (-r {sacCer3,susScr11,ce10,dm3,gg4,canFam2,rn4,bosTau4,mm10,hg19} | -f FASTA)

    optional arguments:
      -h, --help            show this help message and exit
      -i INPUT, --input INPUT
                            FastQ file with reads to map (forward)
      -i2 INPUT_REVERSE, --input-reverse INPUT_REVERSE
                            FastQ file with reads to map (reverse) - if paired end
      -s INPUT_SINGLES, --input-singles INPUT_SINGLES
                            FastQ file with reads to map (singles) - if paired end
                            and unpaired reads exist
      -o OUTPUT, --output OUTPUT
                            Output file/path for results
      --auto-install        Install NGLess if not found in PATH
      --debug               Prints the payload before submitting to ngless
      -r {sacCer3,susScr11,ce10,dm3,gg4,canFam2,rn4,bosTau4,mm10,hg19}, --reference {sacCer3,susScr11,ce10,dm3,gg4,canFam2,rn4,bosTau4,mm10,hg19}
                            Map against a builtin reference
      -f FASTA, --fasta FASTA
                            Map against a given fasta file (will be indexed if
                            index is not available)

## ngless-mapstats.py

This is the equivalent of calling the [mapstats
function](Functions.html#mapstats) from within NGLess. This will take a SAM/BAM
file as input and produce some simple statistics.

    usage: ngless-mapstats.py [-h] -i INPUT -o OUTPUT [--auto-install] [--debug]

    optional arguments:
      -h, --help            show this help message and exit
      -i INPUT, --input INPUT
                            SAM/BAM/CRAM file filter
      -o OUTPUT, --output OUTPUT
                            Output file/path for results
      --auto-install        Install NGLess if not found in PATH
      --debug               Prints the payload before submitting to ngless


## ngless-select.py

This is the equivalent of calling the [select function](Functions.html#select)
from within NGLess:

    usage: ngless-select.py [-h] -i INPUT -o OUTPUT -a {keep_if,drop_if} -c
                            {mapped,unmapped,unique}
                            [{mapped,unmapped,unique} ...] [--auto-install]
                            [--debug]

    optional arguments:
      -h, --help            show this help message and exit
      -i INPUT, --input INPUT
                            SAM/BAM/CRAM file filter
      -o OUTPUT, --output OUTPUT
                            Output file/path for results
      -a {keep_if,drop_if}, --action {keep_if,drop_if}
                            Whether to keep or drop when condition are met
      -c {mapped,unmapped,unique} [{mapped,unmapped,unique} ...], --conditions {mapped,unmapped,unique} [{mapped,unmapped,unique} ...]
                            One or more conditions to filter on
      --auto-install        Install NGLess if not found in PATH
      --debug               Prints the payload before submitting to ngless


## ngless-trim.py

This is equivalent of calling the [preprocess
function](Functions.html#preprocess)  trimming the reads (with either
[substrim](Functions.html#substrim) or [endstrim](Functions.html#endstrim)
depending on the arguments passed. Finally, any (trimmed) reads which are not
of a minimum length are discard.


    usage: ngless-trim.py [-h] -i INPUT -o OUTPUT -m {substrim,endstrim} -q
                          MIN_QUALITY [-d DISCARD] [--auto-install] [--debug]

    optional arguments:
      -h, --help            show this help message and exit
      -i INPUT, --input INPUT
                            FastQ file with reads to trim
      -o OUTPUT, --output OUTPUT
                            Output file/path for results
      -m {substrim,endstrim}, --method {substrim,endstrim}
                            Which trimming method to use
      -q MIN_QUALITY, --min-quality MIN_QUALITY
                            Minimum quality value
      -d DISCARD, --discard DISCARD
                            Discard if shorted than
      --auto-install        Install NGLess if not found in PATH
      --debug               Prints the payload before submitting to ngless


## ngless-unique.py

This is the equivalent of calling the [count function](Functions.html#count)
from within NGLess:

    usage: ngless-unique.py [-h] -i INPUT -o OUTPUT [-c MAX_COPIES]
                            [--auto-install] [--debug]

    optional arguments:
      -h, --help            show this help message and exit
      -i INPUT, --input INPUT
                            FastQ file to filter
      -o OUTPUT, --output OUTPUT
                            Output file/path for results
      -c MAX_COPIES, --max-copies MAX_COPIES
                            Max number of duplicate copies to keep
      --auto-install        Install NGLess if not found in PATH
      --debug               Prints the payload before submitting to ngless

