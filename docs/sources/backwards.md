# List of backwards compatibility fixes

As NGLess uses a version declaration string at the top of script means that
NGLess can change its behaviour depending on the version used in the script.

## &lt; NGLess 1.1

- The way that CIGAR sequence lengths are computed has changed to match
  samtools. This implies that the computation of `min_match_size` and
  `min_identity_pc` have slightly changed.
- Starting in NGLess 1.1, `countfile` reorders its input if necessary.

## &lt; NGLess 0.8

- Select changes how a corner case is handled.

## &lt; NGLess 0.6

- The `count` function now defaults to `include_minus1` being true.

## &lt; NGLess 0.5

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

