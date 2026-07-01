# NGLess is now based on a Rust implementation

Starting with **version 1.6**, NGLess is built on a new implementation written
in [Rust](https://www.rust-lang.org/). Versions up to and including 1.5 were
written in Haskell. Going forward, the Rust implementation is the one that is
developed, maintained, and released.

This page explains what this means for you as a user.

## What does *not* change

The most important point: **for users, essentially nothing changes.**

- The NGLess language is exactly the same. Your scripts do not need to be
  rewritten.
- Version 1.6 is a **best-effort exact mirror of 1.5**. It is designed to
  produce byte-identical output for the same inputs, so results remain
  reproducible across the transition.
- The command-line interface, the standard library, the reference databases,
  and the external tools (bwa, samtools, minimap2, megahit, prodigal, …) all
  behave as before.

The entire NGLess functional test suite passes against the Rust build with
output identical to the Haskell build. This test suite is the contract that
guarantees the transition is reproducible.

## What you should do

Update the version declaration at the top of your scripts from::

    ngless "1.5"

to::

    ngless "1.6"

That is the only change required.

If you continue to declare `ngless "1.5"`, your script will still run, but
NGLess will print a warning recommending that you update to `1.6`. Declaring
version 1.6 removes the warning.

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
| `1.5`            | Runs, with a deprecation warning.               |
| `1.0`–`1.4`      | **Rejected**: use an older NGLess, or update.   |
| `0.x`            | **Rejected**: use an older NGLess, or update.   |

Pre-1.5 semantics carried a long tail of version-specific behaviours that are
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

## Reporting problems

Because 1.6 is meant to be a drop-in replacement for 1.5, any difference in
output between the two (other than differences attributable to external tool
versions) is a bug. If you find one, please report it on the
[issue tracker](https://github.com/ngless-toolkit/ngless/issues), ideally with a
small script and input that reproduces the difference.
