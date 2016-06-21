#include <stdio.h>
void updateCharCount(unsigned int n, char* pc, long int* v) {
    unsigned int i;
    unsigned char* p = (unsigned char*)pc;
    for (i = 0; i < n; ++i) {
        ++v[p[i]];
    }
}
void updateQualityCounts(unsigned int n, char* qsc, long int* v) {
    unsigned int i;
    unsigned char* qs = (unsigned char*)qsc;
    for (i = 0; i < n; ++i) {
        int qi = 256*i + qs[i];
        ++v[qi];
    }
}
