#!/usr/bin/env cwl-runner
# This tool description was generated automatically by argparse2tool ver. 0.4.3-2
# To generate again: $ ./ngless-unique.py -b ./ngless-unique.py --generate_cwl_tool
# Help: $ ./ngless --help_arg2cwl

cwlVersion: "cwl:v1.0"

class: CommandLineTool
baseCommand: ['./ngless-unique.py']

doc: |
  None

inputs:
  
  input:
    type: str
  
    doc: FastQ file to filter
    inputBinding:
      prefix: --input 

  output:
    type: str
  
    doc: Output file/path for results
    inputBinding:
      prefix: --output 

  max_copies:
    type: ["null", str]
    doc: Max number of duplicate copies to keep
    inputBinding:
      prefix: --max-copies 

  debug:
    type: ["null", boolean]
    default: False
    doc: Prints the payload before submitting to ngless
    inputBinding:
      prefix: --debug 


outputs:
    []
