# Software used by NGLess

NGLess internally uses a few other packages to implement specific
functionality. As we believe in giving appropriate credit, these pacakges are
printed in the citation list of any script that uses them.

NGLess version 1.4 uses the following software tools:

- Samtools (used for SAM/BAM handling as well as in the [samtools module](Modules.html): version 2.13
- BWA (used for [map](Functions.html#map)): version 0.7.17
- Minimap (used for [map](Functions.html#map) as an alternative to bwa): version 2.24
- Prodigal (used for [orf\_find](Functions.html#orf_find)): version 2.6.3
- Megahit (used for [assemble](Functions.html#assemble)): version 1.2.9
