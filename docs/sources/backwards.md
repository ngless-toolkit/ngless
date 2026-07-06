# List of language changes

Every NGLess script declares a language version on its first line. This page
documents the changes in behaviour introduced by each version so that you can
understand what a script written for an older version expected.

Note that, in 1.6.0, NGLess supports exactly one language version, `1.6`. Older
versions are documented here for reference, but scripts must be updated to
declare `ngless "1.6"` to run.

## NGLess 1.6

- NGLess is now based on a [Rust implementation](rust.html) (previous versions
  were written in Haskell). Update your version declaration to `ngless "1.6"`;
  earlier versions are no longer supported.

## NGLess 1.4

- The old `motus` module (which supports only motus version 1, which is [a
  reference from 2013](https://www.nature.com/articles/nmeth.2693)) is not
  supported any more. Upgrade to the new [external motus
  module](https://github.com/ngless-toolkit/ngless-contrib/tree/master/motus.ngm)
  if possible.
- `write()` returns the filename used. Before this, it returned nothing.

## NGLess 1.1

- The way that CIGAR sequence lengths are computed has changed to match
  samtools. This implies that the computation of `min_match_size` and
  `min_identity_pc` have slightly changed.
- `countfile` reorders its input if necessary.
- The `count` function now accepts multiple lines of comments at the top of its
  `functional_map` arguments.
- `count()` uses the `sense` argument for strand-specific data. Older scripts
  may use the deprecated boolean `strand` argument, where `strand=True` maps to
  `sense={sense}` and `strand=False` maps to `sense={both}`.
- Older Haskell releases of the `mocat` module exposed `coord_file_to_gtf()` to
  convert MOCAT `.coord` files to GTF. This helper is not part of the current
  Rust standard module surface.

## NGLess 0.8

- The `select` handles a strange corner case differently (it was arguably wrong
  before, but affects very few reads).

## NGLess 0.6

- The `count` function now defaults to `include_minus1` being true.

## NGLess 0.5

- The `preprocess` function now modifies its argument. Older code using

```python
preprocess(input) using |r|:
    ...
```
is automatically treated as:


```python
input = preprocess(input) using |r|:
    ...
```
