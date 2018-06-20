# Methods

Methods are invoked using an object-oriented syntax. For example:

    mapped = select(mapped) using |mr|:
        mr = mr.pe_filter()

They can also take arguments

    mapped = select(mapped) using |mr|:
        mr = mr.filter(min_match_size=30)

## Short reads

Short reads have the following methods:

- `avg_quality()`: the average quality (as a double)
- `fraction_at_least(q)`: the fraction of bases of quality greater or equal to `q`
- `n_to_zero_quality()`: transform the quality scores so that any `N` (or `n`)
  bases in the sequence get a quality of zero.


## Mapped reads

Mapped reads contain several methods. *None of these methods changes its
argument, they return new values.* The typical approach is to reassign the
result to the same variable as before (see examples above).

- `pe_filter`:
    only matches where both mates match are kept.
- `flag`: Takes one of `{mapped}` or `{unmapped}` and returns true if the reads
  were mapped (in a paired-end setting, a read is considered mapped if at least
  one of the mates mapped).
- `some_match`: Takes a reference name and returns True if the read mapped to
  that reference name.
- `allbest`: eliminates matches that are not as good as the best. For NGLess,
  the number of errors (given by the `NM` field) divided by the length of the
  longest match is the fractional distance of a match. Thus, a match with 3
  errors over 100 bp is considered better than a match with 0 errors over
  90bps.

### filter

`filter` takes a mapped read and returns a mapped read according to several
criteria:

- `min_match_size`: minimum match size
- `min_identity_pc`: minimum percent identity (considered over the matching
  location, trimming on the left and right are excluded).
- `max_trim`: maximum number of bases trimmed off the ends. Use 0 to specify
  only global matches.

If more than one test is specified, then they are combined with the AND
operation (i.e., all conditions have to be fulfilled for the test to be true).

The default is to discard mappings that do not pass the test, but it can be
changed with the `action` argument, which must be one of `{drop}` (default:
the read is excluded from the output), or `{unmatch}` (the read is changed so
that it no longer reports matching).

You can pass the flag `reverse` (i.e., `reverse=True`) to reverse the sign of
the test.

