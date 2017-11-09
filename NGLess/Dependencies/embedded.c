#ifndef NO_EMBED_SAMTOOLS_BWA
#include "samtools_data.c"
#include "prodigal_data.c"
#include "bwa_data.c"
#include "megahit_data.c"

const unsigned char* get_samtools_data () { return samtools_1_4_ngless_0_5_1_samtools_static; }
unsigned int get_samtools_len () { return samtools_1_4_ngless_0_5_1_samtools_static_len; }

const unsigned char* get_prodigal_data () { return prodigal_1_4_ngless_0_5_1_prodigal_static; }
unsigned int get_prodigal_len () { return prodigal_1_4_ngless_0_5_1_prodigal_static_len; }

const unsigned char* get_bwa_data () { return bwa_0_7_15_ngless_0_5_1_bwa_static; }
unsigned int get_bwa_len () { return bwa_0_7_15_ngless_0_5_1_bwa_static_len; }

const unsigned char* get_megahit_data() { return megahit_1_1_1_megahit_packaged_tar_gz; }
unsigned int get_megahit_len() { return megahit_1_1_1_megahit_packaged_tar_gz_len; }

#else
const unsigned char* get_samtools_data () { return ""; }
unsigned int get_samtools_len () { return 0; }

const unsigned char* get_prodigal_data () { return ""; }
unsigned int get_prodigal_len () { return 0; }

const unsigned char* get_bwa_data () { return ""; }
unsigned int get_bwa_len () { return 0; }

const unsigned char* get_megahit_data() { return ""; }
unsigned int get_megahit_len() { return 0; }

#endif
