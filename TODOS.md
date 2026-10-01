# TODOs

This document is a list of planned tasks and features for the project.

- Add Python-like negative indexing support for reads and lists
- Update the demos (`gut-short`, `ocean-short`) on the resources server: their scripts (`gut-demo.ngl`, `ocean-demo.ngl`) declare `ngless "1.1"` (and the gut one uses `import "motus"`), so they no longer run, and the ocean read files (`*_1.fastq.gz.short.fq.gz`) do not match the paired-end naming rules, so they load as single-end
