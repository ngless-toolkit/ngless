# NGLess is now based on a Rust implementation

Starting with **version 1.6**, NGLess is built on a new implementation written
in [Rust](https://www.rust-lang.org/). Versions up to and including 1.5 were
written in Haskell. Going forward, the Rust implementation is the one that is
developed, maintained, and released.

This page explains what this means for you as a user.

## What does *not* change

The most important point: **for users, very little changes.**

- The NGLess language is the same. Most scripts only need their version
  declaration updated (see below for the exceptions).
- Version 1.6 was designed to produce equivalent output for the same inputs as
  1.5, so results remain reproducible across the transition.
- The command-line interface, the standard library, the reference databases,
  and the external tools (bwa, samtools, minimap2, megahit, prodigal, …) all
  behave as before.

The NGLess functional test suite, whose expected outputs were produced by the
Haskell build, passes against the Rust build. Going forward, it ensures that
output stays stable across releases.

## What you should do

Update the version declaration at the top of your scripts from::

    ngless "1.5"

to::

    ngless "1.6"

For most scripts, that is the only change required (see *Known changes* below
for the exceptions).

The Rust build supports a **single** language version, `1.6`. Declaring any
other version — including `ngless "1.5"` — is now a **hard error**, not a
warning. Update the version statement at the top of your scripts to `ngless
"1.6"`.

### Module imports

The built-in modules (`parallel`, `samtools`, `mocat`, …) now track the ngless
version too. Going forward, import them at version `1.6`::

    import "parallel" version "1.6"
    import "samtools" version "1.6"

Older module versions are still accepted (with the latest behaviour), but
importing one prints a deprecation warning suggesting you update to `1.6`.

## Version support

| Declared version | Rust build (1.6+) behaviour                    |
|------------------|------------------------------------------------|
| `1.6`            | Runs (current version).                         |
| `1.5`            | **Rejected**: update the script to `1.6`.       |
| `1.0`–`1.4`      | **Rejected**: use an older NGLess, or update.   |
| `0.x`            | **Rejected**: use an older NGLess, or update.   |

Pre-1.6 semantics carried a long tail of version-specific behaviours that are
not reproduced by the Rust implementation. If you need to run an old script
verbatim without updating it, use an older NGLess release (up to 1.5, which is
the last Haskell release).

## Why the switch to Rust?

The motivation is **not** performance — the Haskell implementation already
streamed large files efficiently. The drivers are practical:

- **Build & maintenance.** The Haskell toolchain (GHC + Stack + Nix +
  `haskell.nix`) is heavy and slow to onboard. Rust's toolchain is simpler.
- **Contributors & ecosystem.** Rust has a larger contributor base and a strong
  bioinformatics crate ecosystem.
- **Distribution.** A single static binary is simpler to build and ship.

As a side effect, the rewrite eliminates all the bundled C/C++/FFI code that the
Haskell build carried.

## Known changes

A few behaviours differ from the Haskell implementation (1.5):

- **Binary operators follow the usual precedence rules** (since 1.6.1). `*`
  binds tighter than `+`, `-`, and `</>`, which bind tighter than the
  comparisons, and operators of equal precedence group to the left (see
  [the language description](Language.md)). In 1.5 (and 1.6.0), operators
  had no relative precedence and grouped to the right, so `2 * 3 + 1` was 8
  (it is now 7). Expressions that only use a single operator, or that use
  parentheses, are not affected. The binary subtraction operator (`x - 1`) is
  also new in 1.6.1.

- **`count()` rejects ambiguous annotation sources.** The annotation to use is
  chosen from `features=["seqname"]`, `gff_file`, `functional_map`, or
  `reference`. Passing more than one of these is now an error, reported before
  the pipeline runs. Previously all but one were silently ignored (the Haskell
  implementation only errored on the `gff_file` + `functional_map` combination).
  Pass exactly one annotation source so that no argument is silently dropped.
- **Error messages are not exactly the same.** The Rust implementation has a
  different error reporting mechanism, so the text of error messages may
  differ.
- **Direct indexing of a function-call result is not supported yet.** Write
  the result to a variable first, then index that variable. For example, use
  `xs = readlines("samples.txt")` followed by `sample = xs[0]` instead of
  `sample = readlines("samples.txt")[0]`.
- **The deprecated `strand` argument to `count()` is no longer accepted.** Use
  `sense` (`{both}`/`{sense}`/`{antisense}`) instead; `strand=True` is
  equivalent to `sense={sense}`. Passing `strand` is now an unknown-argument
  error.
- **The `soap` module has been removed.** `import "soap"` (and the SOAP mapper)
  is no longer available and is rejected at import time. Use one of the
  supported mappers (bwa or minimap2).
- **`import "motus"` is rejected.** It referred to the obsolete mOTUs v1
  module. Use the [external mOTUs module](motus3.md) with `local import`.
- **The `{hash}` values are different.** The hash written by
  `auto_comments=[{hash}]` (and used to name the `parallel` module's lock
  directories) is still deterministic and content-addressed, but its values
  differ from 1.5. It is an internal identifier, not a value that is stable
  across versions.

## Reporting problems

Apart from the changes listed above, 1.6 is meant to produce the same results
as 1.5, so an unexplained difference in output between the two (other than
differences attributable to external tool versions) is likely a bug. If you find one, please report it on the
[issue tracker](https://github.com/ngless-toolkit/ngless/issues), ideally with a
small script and input that reproduces the difference.
