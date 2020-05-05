# Counting in NGLess

The `count()` function takes a `MappedReadSet` (the logical equivalent of a
SAM/BAM file) and  summarizes the information therein. The `count()` function
can perform three types of operation, depending on the `features` argument:

1. `seqname`: only the `MappedReadSet` is necessary.
2. Using a GFF/GTF file.
3. Using a `functional_map` (TSV) file

Option \#1 is the simplest to understand: just summarize based on the names of
the sequences. This is appropriate for obtaining per-gene abundances from gene
catalogs. Options \#2 and \#3 are similar: for each `MappedRead`, `count()`
will use the passed reference to map to a set of features and summarize those.
Using the GFF format is much more flexible and allows for a lot more filtering,
but also _significantly costlier in time and memory_. At a high-level, the
process is similar:

For example, if you have an insert (either a paired-end or single-end
short-read) that mapped to a gene called `G1` and that gene is annotated with
two gene ontology terms, both will be considered and their counts adjusted. If
the insert mapped to multiple genes, then all the terms will be considered, but
as a set: if an insert is mappped to `G1`, which has two GO annotations and
also to `G2` which has the same GO annotations (which is very frequent), then
those annotations will be counted a single time.

How counts are adjusted in the presence of multiple annotations is defined by
the `multiple` argument. Generally, for obtaining gene abundances, distribution
of multiple mappers is the best (using `multiple={dist1}`), while for
_functional annotations_, you want to count them all (using `multiple={all1}`).
This implies that the functional annotations will sum to a higher value than
the number of reads. This may seem strange at first, but it is the intended
behaviour.

See also the [full description of all count arguments in the API
docs](Functions.html#count).

## A TSV (tab-separated values) file for use in the `functional_map` argument

The file consists of a header and content.

### TSV header

The simplest header is just a single line of tab separated column headers. That
line _may_ start with a `#` sign, which is ignored. Alternatively, a multi-line
header consists of multiple lines starting with `#`. The last one of these will
be parsed as the header.


#### Examples of TSV headers

ALl the 

1. Simple, 1 line header, with a `#` sign

```
#geneID	feat1	feat2	feat3
G1	ann	ann	ann
G2	ann	ann	ann
```

2. Simple, 1 line header, without a `#` sign

```
geneID	feat1	feat2	feat3
G1	ann	ann	ann
G2	ann	ann	ann
```

3. Multi-line header, `#` signs are required

```
# My comment can span multiple lines
# The last one of these is the header!
#geneID	feat1	feat2	feat3
G1	ann	ann	ann
G2	ann	ann	ann
```

_Note_: format \#3 is only supported in NGLess version 1.1 and above

### TSV content


Values can be

1. empty.
2. lists of entries, separated by either `,` or `|` characters.

```
#geneID	feat1	feat2	feat3
G1	a1,a2	b	c
G2	a1|a3		c
```

In this case, the mappings are:

- `G1` has the properties `a1` and `a2` in the `feat1` feature; `b` in the
  `feat2` feature; and `c` in the `col3`
- `G2` has the properties `a1` and `a3` in the `feat1` feature; and `c` in the
  `col3`. There is no `feat2` associated with `G2` and, from `feat2`'s
  point-of-view, inserts mapped to `G2` are considered unmapped

As of NGLess 1.1, _spaces are not allowed_: i.e., `a, b` is the feature `a` and
the feature ` b` (space followed by b). This is arguably sub-optimal.

