#!/usr/bin/env bash

# Write a deterministic FASTA to the --contigs=<path> output argument, ignoring
# the input read file. This exercises the `sequenceset` external-module return type.
out=""
for a in "$@"; do
    case "$a" in
        --contigs=*) out="${a#--contigs=}" ;;
    esac
done

if [ -z "$out" ]; then
    echo "no --contigs argument given" >&2
    exit 1
fi

cat > "$out" <<'EOF'
>contig1
ACGTACGTACGT
>contig2
TTTTGGGGCCCC
EOF
