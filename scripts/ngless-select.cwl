#!/usr/bin/env cwl-runner
# This tool description was generated automatically by argparse2tool ver. 0.4.3-2
# To generate again: $ ./ngless-select.py -b ./ngless-select.py --generate_cwl_tool
# Help: $ ./ngless --help_arg2cwl

cwlVersion: "cwl:v1.0"

class: CommandLineTool
baseCommand: ['./ngless-select.py']

doc: |
  None

inputs:
  
  input:
    type: str
  
    doc: SAM/BAM/CRAM file filter
    inputBinding:
      prefix: --input 

  output:
    type: str
  
    doc: Output file/path for results
    inputBinding:
      prefix: --output 

  action:
    type:
      type: enum
      symbols: ['keep_if', 'drop_if']
    doc: Whether to keep or drop when condition are met
    inputBinding:
      prefix: --action 

  conditions:
    type:
      type: enum
      symbols: ['mapped', 'unmapped', 'unique']
    doc: One or more conditions to filter on
    inputBinding:
      prefix: --conditions 

  debug:
    type: ["null", boolean]
    default: False
    doc: Prints the payload before submitting to ngless
    inputBinding:
      prefix: --debug 


outputs:
    []
