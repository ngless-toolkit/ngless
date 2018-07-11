# NG-meta-profiler

NG-meta-profiler is a collection of predefined pipelines for processing shotgun
metagenomes.

1. `human-gut.ngl` for human gut samples
2. `marine.ngl` for marine samples
3. `mouse-gut.ngl` for mouse gut samples
4. `dog-gut.ngl` for dog gut samples
5. `pig-gut.ngl` for pig gut samples

These are predefined, but users are encouraged to adapt them to their specific
needs.

## INSTALL

1. install [ngless](install.html)
2. install ng-meta-profiler by downloading the appropriate pipeline from github:
   [https://github.com/ngless-toolkit/ng-meta-profiler](https://github.com/ngless-toolkit/ng-meta-profiler)

## USAGE

To use the profiler, select the appropriate script (e.g., `human-gut.ngl`
for human gut samples), put all the FastQ files from the same sample in the
same directory (`INPUT-DIRECTORY`) with the extension `.fq.gz` or `fastq.gz`
and run:

    ngless human-gut.ngl INPUT-DIRECTORY OUTPUT-DIRECTORY

