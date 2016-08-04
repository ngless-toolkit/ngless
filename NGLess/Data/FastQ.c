#include <stdio.h>
void updateCharCount(unsigned int n, char* pc, long int* v) {
    unsigned int i;
    unsigned char* p = (unsigned char*)pc;
    for (i = 0; i < n; ++i) {
        ++v[p[i]];
    }
}
