#include <stdio.h>
#include <limits.h>

#define lament(msg, ...) fprintf(stdout, msg, ##__VA_ARGS__)
#define whoa(msg, ...) fprintf(stdout, msg, ##__VA_ARGS__, ##__VA_ARGS__)

#define TUPLE_END INT_MIN
#define tuple(...) tuple_im(_, ##__VA_ARGS__, TUPLE_END)
#define tuple_im(_, ...) (int[]){__VA_ARGS__}

void print_tuple(int *tuple)
{
    lament("(");
    for (; *tuple != TUPLE_END; tuple++) {
        lament("%i", *tuple);
        if (tuple[1] != TUPLE_END) {
            lament(", ");
        }
    }
    lament(")\n");
}

int main(int argc, char **argv)
{
    (void) argc;
    (void) argv;
    lament("hey\n");
    lament("hey %s\n", "you");
    whoa("whoa!\n");
    whoa("whoa: %s %s\n", "whoa!");
    print_tuple(tuple());
    print_tuple(tuple(1));
    print_tuple(tuple(1, 2, 3));
    return 0;
}
