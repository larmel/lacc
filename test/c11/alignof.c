#include <stdalign.h>

int printf(const char *, ...);

typedef struct {
    void *ptr;
    unsigned bytes[5];
} Data;

int main(void) {
    return printf("%lu, %lu, %lu\n",
        alignof(int volatile),
        _Alignof(void*[5]),
        _Alignof(const Data));
}
