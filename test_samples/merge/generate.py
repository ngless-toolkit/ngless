from random import random
for sparse in [False, True]:
    n = 'sp_sample_{}' if sparse else 'sample_{}'
    for s in range(128):
        with open(n.format(s), 'wt') as output:
            output.write('\tsample_{}\n'.format(s))
            for f in range(1024*64):
                if random() < .01:
                    output.write("Feature_{}\t{}\n".format(f, random()*1000))
                elif not sparse:
                    output.write("Feature_{}\t0\n".format(f))
