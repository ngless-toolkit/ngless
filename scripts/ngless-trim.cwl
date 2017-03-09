#!/usr/bin/env cwl-runner
# This tool description was generated automatically by argparse2tool ver. 0.4.3-2
# To generate again: $ ./ngless-trim.py -b ./ngless-trim.py --generate_cwl_tool
# Help: $ ./ngless --help_arg2cwl

cwlVersion: "cwl:v1.0"

class: CommandLineTool
baseCommand: ['./ngless-trim.py']

doc: |
  None

inputs:
  
  input:
    type: str
  
    doc: FastQ file with reads to trim
    inputBinding:
      prefix: --input 

  output:
    type: str
  
    doc: Output file/path for results
    inputBinding:
      prefix: --output 

  method:
    type:
      type: enum
      symbols: ['substrim', 'endstrim']
    doc: Which trimming method to use
    inputBinding:
      prefix: --method 

  min_quality:
    type: ["null", int]
    doc: Minimum quality value
    inputBinding:
      prefix: --min-quality 

  discard:
    type: ["null", int]
    default: 50
    doc: Discard if shorted than
    inputBinding:
      prefix: --discard 

  debug:
    type: ["null", boolean]
    default: False
    doc: Prints the payload before submitting to ngless
    inputBinding:
      prefix: --debug 


outputs:
    []
