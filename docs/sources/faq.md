# Frequently Asked Questions

This is a list of questions we have regularly gotten on the project. See below
for questions about the ngless language.

## Why a new domain-specific language instead of a library in Python (or another existing language)?

First of all, you can actually use NGLess through Python, using
[NGLessPy](https://github.com/ngless-toolkit/nglesspy).

However, the native mode of NGLess is using its internal DSL (domain specific
langugage).  There are several advantages to this approach:

1. Fast error checking which can speed up the development process. For example,
   static type checking, which is known to many programmer. In general, we do a
   lot of error checking before even starting interpretation. We perform syntax
   and error checking, but we can also check some conditions that can be tricky
   to express with simple types only (e.g., certain parameter combinations can
   be illegal). We also pre-check all the input files (so even if you only use
   a particular file in step 5 of your process, we check if it exists even
   before running steps 1 through 4). We even do some things like: if you use
   step 1 to compute to name of the input file that will be used in step 5, we
   will check it immediately after step 1. Same for output files. If you issue
   a `write()` call using `output/results.txt` as your output filename, we will
   check if a directory named output exists and is writable. We also try to be
   helpful in the error messages (mispelled a parameter value? Here's an error,
   but also my best guess of what you meant + all legal values). I really care
   about error messages.

2. By controlling the environment more than would be typical with a Python
   library (or any other language), we can also get some reproducibility
   guarantees. Note too that we declare the version of every script so that we
   can update the interpreter in the future without silently changing the
   behaviour of older ones.

3. Using a domain specific language makes the resulting scripts very readable
   even for non-experts as there is little boilerplate.

4. Finally, we needed the result to be fast and languages such as Python often
   add a lot of overhead.

## Is the language extensible?

Yes.

While the basic types and syntax of the language are fixed, it is not hard to
add external modules that introduce new functions. These can be described with
a YAML file and can use any command line tool.

Add new model organisms can similarly be done with simple YAML file.

More advanced extensions can be done in Haskell, but this is considered a
solution for advanced users.

## Couldn't you just use Docker/[Bioboxes](http://bioboxes.org/)?

Short answer: Bioboxes gets us part of the way there, but not all of it;
however, if we think of these technologies as complements, we might get more
out of them.

Longer answer:

Several of the goals of ngless can be fulfilled with a technology such as
bioboxes. Namely, we can obtain reproducibility of computation, including
across platforms using bioboxes without having to bother with ngless. However,
the result is less readable than an ngless script, which is another important
goal of ngless. An ngless script can be easily be submitted as supplemented
methods to a journal publication and even be easily scrutinized by a
knowledgeable reviewer in an easier way than a Docker container.

Furthermore, the fact that we work with a smaller domain than a Docker-based
solution (we only care about NGS) means that we can provide the users a better
experience than is possible with a generic tool. In particular, when the user
makes a mistake (and all users will make mistakes), we can diagnose it faster
and provide a better error message than is possible to do with Bioboxes.

## What is the relationship of ngless to the [Common Workflow Language](http://common-workflow-language.github.io/)?

Like for the question above, we consider ngless to be related to but not
overlapping with the CWL (Common Workflow Language).

In particular, much of functionality of ngless can also be accessed in CWL
workflow, using [our command line wrappers](command-line-wrappers.html) all of
which have CWL wrappers.

Additionally, (with some limitations), you can embedded a generic NGLess script
within a larger CWL workflow by using the `--export-cwl` functionality. For
example, to automatically generate a wrapper for a script called
`my-script.ngl`, call:

    ngless --export-cwl=wrapper.cwl my-script.ngl

The automatically generated `wrapper.cwl` file can now be used as a CWL tool
within a larger pipeline. See more in the [CWL page](cwl.html).

## How does ngless interact with job schedulers and HPC clusters?

Generally speaking, it does not. It can be used with HPC clusters, whereby you
simply run a job that calls the ngless binary.

The [parallel
module](https://ngless.embl.de/stdlib.html?highlight=parallel#parallel-module)
can be used to split large jobs in many tasks, so that you can run multiple
ngless instances and they collaborate. It is written such that does not depend
on the HPC scheduler and can, thus, be used in any HPC system (or even, for
smaller jobs, on a single machine).

## Questions about the ngless language

## Can I pass command line arguments to a script?

Yes, you can. Just add them as additional arguments and they will be available
inside your script as `ARGV`.

## What are symbols (in the ngless language)?

If you are familiar with the concept, you can think of them as `enums` in other
languages.

Whenever a symbol is used in the argument to a function, this means that that
function takes only one of a small number of possible symbols for that
argument. This improves error checking and readibility.

## Does the select function work on inserts (considering both mates) or per-read (treating the data as single-ended)?

By default, `select` considers the insert as a whole, but you can have it
consider each read as single-end by using setting the `paired` argument to
`False`.

