#ifndef NO_EMBED_SAMTOOLS_BWA
#include "samtools_data.c"
#include "bwa_data.c"
const unsigned char* get_samtools_data () { return samtools_1_4_ngless_samtools_static; }
unsigned int get_samtools_len () { return samtools_1_4_ngless_samtools_static_len; }

const unsigned char* get_bwa_data () { return bwa_0_7_15_ngless_bwa_static; }
unsigned int get_bwa_len () { return bwa_0_7_15_ngless_bwa_static_len; }

#else
const unsigned char* get_samtools_data () { return ""; }
unsigned int get_samtools_len () { return 0; }

const unsigned char* get_bwa_data () { return ""; }
unsigned int get_bwa_len () { return 0; }

#endif
