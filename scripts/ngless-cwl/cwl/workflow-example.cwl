#!/usr/bin/env cwl-runner

cwlVersion: v1.0

requirements:
- $import: ngl-types.yml

class: Workflow
inputs:
  start:
    type: File
    label: reads.fq
    doc: FastQ file to process
  trim_output_name:
    type: string
    label: Trimmed reads
    doc: Temporary file with trimmed reads
  trim_method:
    type: ngl-types.yml#trim_method
    label: Trimming method
    doc: Given a read, keep the longest segment above a quality threshold (substrim) or trim from both ends (endstrim)
  trim_qual:
    type: int
    label: Trim quality cutoff
    doc: Quality trimming threshold
  trim_discard:
    type: int
    label: Trim discard cutoff
    doc: Discard if shorter than
  map_output_name:
    type: string
    label: Mapping result filename
    doc: Name of file containing mapping results
  stats_output_name:
    type: string
    label: Mapping statistics filename
    doc: Name of file containing mapping summary statistics
  ref:
    type: File
    label: Fasta database
    doc: Fasta file with reads to map against

outputs:
  mapstats_out:
    type: File
    label: Mapping result
    doc: Stats file with summary measures of mapping
    outputSource: mapstats/output_file
  map_out:
    type: File
    label: Mapping statistics
    doc: SAM/BAM/CRAM file with the mapping result
    outputSource: map/output_file

steps:
  trim:
    run: ngless-trim.cwl
    in:
      input: start
      method: trim_method
      min_quality: trim_qual
      discard: trim_discard
      output: trim_output_name
    out: [output_file]

  map:
    run: ngless-map.cwl
    in:
      input: trim/output_file
      fasta: ref
      output: map_output_name
    out: [output_file]

  mapstats:
    run: ngless-mapstats.cwl
    in:
      input: map/output_file
      output: stats_output_name
    out: [output_file]
