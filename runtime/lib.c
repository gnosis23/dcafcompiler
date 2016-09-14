#include <stdlib.h>
#include <stdio.h>

int get_int() {
    int c;
    printf("input int > ");
    scanf("%d", &c);
    return c;
}

void __checkArrayIndex(int index, int arraySize) {
    if (index < 0 || index >= arraySize) {
        fprintf(stderr, "array index out of range.\n");
        exit(-1);
    }
}
