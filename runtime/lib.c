#include <stdlib.h>
#include <stdio.h>

long get_int() {
    int c;
    printf("input int > ");
    scanf("%ld", &c);
    return c;
}

void __checkArrayIndex(long index, long arraySize) {
    if (index < 0 || index >= arraySize) {
        fprintf(stderr, "array index out of range.\n");
        exit(-1);
    }
}
