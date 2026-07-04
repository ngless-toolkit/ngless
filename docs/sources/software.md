# Software used by NGLess

NGLess uses a few other packages to implement specific functionality. As we
believe in giving appropriate credit, these packages are
printed in the citation list of any script that uses them.

NGLess 1.6 resolves external tools from environment variables such as
`NGLESS_BWA_BIN` and `NGLESS_SAMTOOLS_BIN` or, if those are not set, from
`PATH`. The exact tool version is therefore the one installed in your
environment. Commonly used tools are:

- Samtools (used for SAM/BAM handling as well as in the [samtools module](stdlib.html#samtools-module))
- BWA (used by [map](Functions.html#map) by default)
- Minimap2 (used by [map](Functions.html#map) when selected as an alternative mapper)
- Prodigal (used by [orf_find](Functions.html#orf-find))
- Megahit (used by [assemble](Functions.html#assemble))
