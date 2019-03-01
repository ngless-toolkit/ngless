#!/usr/bin/env bash
cat >> stack.yaml <<EOF

ghc-options:
  "\$everything": "-O0"

EOF
