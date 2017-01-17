# Frequently Asked Questions

This is a list of questions we have regularly gotten on the project:

## Why a new domain-specific language instead of a library in Python (or another existing language)?

There are several advantages:

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

2. Another important advantage is that Haskell has a compiled implementation
   and performance matters. I use Python all the time and like the ecosystem a
   lot, but for some of the things that ngless does, it'd just be too slow (and
   if using PyPy, I'd miss out on numpy, so that's not very feasible either).

3. By controlling the environment more than would be typical with a Python
   library (or any other language), we can also get some reproducibility
   guarantees. Note too that we declare the version of every script so that we
   can update the interpreter in the future without silently changing the
   behaviour of older ones.

4. Using a domain specific language makes the resulting scripts very readable
   even for non-experts as there is little boilerplate.

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

However, it may be worthwhile to consider Docker-technologies as *being used by
ngless* to make distribution easier. We plan to distribute a Docker image
version of ngless, which can contain external modules as well, thus solving a
whole set of distribution problems.

## What is the relationship of ngless to the [Common Workflow Language](http://common-workflow-language.github.io/)?

Like for the question above, we consider ngless to be related to but not
overlapping with the CWL (Common Workflow Language).

In particular, we can envision using the CWL as a target of an ngless script:
that is, we can imagine a future implementation of ngless that takes an ngless
script and outputs a CWL one, which can be used to run the pipeline. In this
analogy, CWL functions as a common assembly language, while ngless is the
high-level language which compiles to CWL.

