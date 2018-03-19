#ifndef NO_EMBED_SAMTOOLS_BWA
#include "samtools_data.c"
#include "prodigal_data.c"
#include "bwa_data.c"
#include "megahit_data.c"
#include "minimap2_data.c"

const unsigned char* get_samtools_data () { return ngless_samtools_static; }
unsigned int get_samtools_len () { return ngless_samtools_static_len; }

const unsigned char* get_prodigal_data () { return ngless_prodigal_static; }
unsigned int get_prodigal_len () { return ngless_prodigal_static_len; }

const unsigned char* get_bwa_data () { return ngless_bwa_static; }
unsigned int get_bwa_len () { return ngless_bwa_static_len; }

const unsigned char* get_megahit_data() { return megahit_packaged_tar_gz; }
unsigned int get_megahit_len() { return megahit_packaged_tar_gz_len; }

const unsigned char* get_minimap2_data() { return ngless_minimap2_static; }
unsigned int get_minimap2_len() { return ngless_minimap2_static_len; }

#else

const unsigned char* get_samtools_data () { return ""; }
unsigned int get_samtools_len () { return 0; }

const unsigned char* get_prodigal_data () { return ""; }
unsigned int get_prodigal_len () { return 0; }

const unsigned char* get_bwa_data () { return ""; }
unsigned int get_bwa_len () { return 0; }

const unsigned char* get_megahit_data() { return ""; }
unsigned int get_megahit_len() { return 0; }

const unsigned char* get_minimap2_data() { return ""; }
unsigned int get_minimap2_len() { return 0; }

#endif
