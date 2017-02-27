#!/usr/bin/env cwl-runner
# This tool description was generated automatically by argparse2tool ver. 0.4.3-2
# To generate again: $ ./ngless-count.py -b ./ngless-count.py --generate_cwl_tool
# Help: $ ./ngless --help_arg2cwl

cwlVersion: "cwl:v1.0"

class: CommandLineTool
baseCommand: ['./ngless-count.py']

doc: |
  None

inputs:
  
  input:
    type: str
  
    doc: SAM/BAM/CRAM file to count reads on
    inputBinding:
      prefix: --input 

  output:
    type: str
  
    doc: Output file/path for results
    inputBinding:
      prefix: --output 

  multiple:
    type:
    - "null"
    - type: enum
      symbols: ['dist1', 'all1', '1overN', 'unique_only']
    doc: Output file/path for results
    inputBinding:
      prefix: --multiple 

outputs:
    []
