# Frequently Asked Questions

This is a list of questions we have regularly gotten on the project:

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

