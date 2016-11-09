from collections import defaultdict
import os
import math
try:
    import argparse
except ImportError: # Python 2.6
    import old_argparse as argparse

parser = argparse.ArgumentParser(description='Summarize motus table')
parser.add_argument('input_counts',
                    help='gene counts from ngless')
parser.add_argument('--ofile', dest='ofile',
                    help='output file name')

args = parser.parse_args()

output_file = open(args.ofile, 'w')
write = output_file.write


# This could probably be done faster & simpler with a few numpy + pandas
# functions, but we prefer to not depend on those packages and work with
# standard Python only

MIN_PREVALENCE = 2

def mean_or_na(vs):
    '''compute mean of argument, while respecting the prevalence and minimum
    abundance rules.

    Namely: only non-zero values are counted, but we require at least
    `MIN_PREVALENCE` of them and that the average be at least 1'''

    prev = sum((v > 0) for v in vs)
    if prev < MIN_PREVALENCE:
        return 0.0
    m = sum(vs)/float(prev)
    if m < 1.:
        return 0.0
    return m


percog = {}
for i,line in enumerate(open(args.input_counts)):
    tokens = line.rstrip().split('\t')
    if i == 0:
        headers = tokens[1:]
    else:
        counts = [math.floor(float(v)) for v in tokens[1:]]
        percog[tokens[0]] = counts

summary = defaultdict(list)

workdir = os.environ.get('NGLESS_MODULE_DIR', '.')

for i,line in enumerate(open(workdir + '/data/mOTU.v1.padded.motu.linkage.map', 'r')):
    if i == 0:
        continue
    tokens = line.rstrip('\n').split('\t')
    if tokens[0] in percog:
        summary[tokens[10]].append(percog[tokens[0]])

write("\t")
write("\t".join(headers))
write("\n")

for k in sorted(summary):
    if k == 'NA':
        continue
    counts = summary[k]
    s = []
    for i,h in enumerate(headers):
        vs = [v[i] for v in counts]
        s.append(mean_or_na(vs))

    if sum(s):
        write(k)
        write("\t")
        write("\t".join(map(str,s)))
        write("\n")
