#!/usr/bin/env bash

for g in g.*.gz; do
    out=$(echo $g | sed 's#^g.expected#output#')
    if ! diff -u <(zcat $g) <(zcat $out); then
	>&2 echo "$g and $out differ in content" 
        exit 1
    fi
done
