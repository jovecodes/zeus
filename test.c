// ----------- FORWARD DECLARATIONS (TYPES) -----------
struct Foo;

// ----------- FORWARD DECLARATIONS (FUNCTIONS) -----------
void take_foo(struct Foo* foo);
void take_int(int a);
int main();

// ----------- PROGRAM CODE -----------
#include <stdio.h>
struct Foo {
int bar ;
};
void take_foo(struct Foo* foo) {
    foo->bar += 10;
}

void take_int(int a) {
    printf("a = %d\n", a);
}

int main() {
    int x = 20;
    struct Foo foo = {0};
    take_foo((&foo));
    take_int(foo.bar);
    printf("Counting up to %d\n", x);
    int i = 1;
    while ((i <= x)) {
        printf("%d, ", i);
        if (((i % 10) == 0)) {
            printf("\n");
        }

        i += 1;
    }

    return 0;
}

