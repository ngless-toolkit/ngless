from collections import defaultdict
import gzip
import sys

input_counts = sys.argv[1]
write = sys.stdout.write


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
for i,line in enumerate(open(input_counts)):
    tokens = line.rstrip().split('\t')
    if i == 0:
        headers = tokens[1:]
    else:
        counts = [float(v) for v in tokens[1:]]
        percog[tokens[0]] = counts

summary = defaultdict(list)

for i,line in enumerate(gzip.open('mOTU.nr.padded.motu.linkage.map.gz', 'r')):
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
