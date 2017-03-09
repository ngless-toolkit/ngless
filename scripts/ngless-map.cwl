#!/usr/bin/env cwl-runner
# This tool description was generated automatically by argparse2tool ver. 0.4.3-2
# To generate again: $ ./ngless-map.py -b ./ngless-map.py --generate_cwl_tool
# Help: $ ./ngless --help_arg2cwl

cwlVersion: "cwl:v1.0"

class: CommandLineTool
baseCommand: ['./ngless-map.py']

doc: |
  None

inputs:
  
  input:
    type: str
  
    doc: FastQ file with reads to map (forward)
    inputBinding:
      prefix: --input 

  input_reverse:
    type: ["null", str]
    doc: FastQ file with reads to map (reverse) - if paired end
    inputBinding:
      prefix: --input-reverse 

  input_singles:
    type: ["null", str]
    doc: FastQ file with reads to map (singles) - if paired end and unpaired reads exist
    inputBinding:
      prefix: --input-singles 

  output:
    type: str
  
    doc: Output file/path for results
    inputBinding:
      prefix: --output 

  debug:
    type: ["null", boolean]
    default: False
    doc: Prints the payload before submitting to ngless
    inputBinding:
      prefix: --debug 

  reference:
    type:
    - "null"
    - type: enum
      symbols: ['sacCer3', 'ce10', 'dm3', 'gg4', 'canFam2', 'rn4', 'bosTau4', 'mm10', 'hg19']
    doc: Map against a builtin reference
    inputBinding:
      prefix: --reference 

  fasta:
    type: ["null", str]
    doc: Map against a given fasta file (will be indexed if index is not available)
    inputBinding:
      prefix: --fasta 


outputs:
    []
