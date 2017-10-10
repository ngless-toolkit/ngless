# Introduction

## Motivation

Nearly all next generation sequence (NGS) applications rely on sequence
alignment as the first analysis step. Before the alignment they require some
kind of pre-processing of data, that is always dependent on the researcher
interest. Our objective is to allow the creation of a pipeline of work for all
the first phase of NGS analysis until the point (inclusive) of annotation. We
want to do this while achieving the following goals:

- Ease the development of NGS Tools;
- Enable an easy identification of errors;
- Easily reproduce an experiment;
- Easy configuration and execution of workflows;
- Exploit available computational resources.

## Target Users

Bioinformaticians working in a wetlab setting. Every serious biological lab in
the world now needs to hire at least one.  They know programming (at least
basic programming), but are not method developers.

The tool can still be useful for more advanced users.


## Basic Properties

- The syntax is a pythonesque syntax with Ruby-like blocks.
- The types are statically and strictly.
- Types are implicit, but limited language allows for type inference and checking.
- Quality control is implicit and mandatory (you get it for free)
- Types are domain types (biological).
