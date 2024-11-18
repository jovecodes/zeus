// ----------- PRELUDE -----------
typedef struct {
    void *items;
    long count;
} ZeusArray___ ;

typedef struct {
    void *items;
    long count;
    long capacity;
} DynamicArray;

// ----------- FORWARD DECLARATIONS (TYPES) -----------
struct Foo {
int bar ;
};

// ----------- FORWARD DECLARATIONS (FUNCTIONS) -----------
void take_foo_ptr(struct Foo* foo);
void take_foo(struct Foo foo);
void take_int(int a);
void take_array(ZeusArray___ arr);
int main();

// ----------- ARRAY DATA -----------
static const int ARRAY_DATA__824_844[] = {10, 20, 30, };

// ----------- PROGRAM CODE -----------
#include <stdio.h>
;
void take_foo_ptr(struct Foo* foo) {
    foo->bar += 10;
}

void take_foo(struct Foo foo) {
    printf("taken foo: %d\n", foo.bar);
}

void take_int(int a) {
    printf("a = %d\n", a);
}

void take_array(ZeusArray___ arr) {
    int i = 0;
    while ((i < arr.count)) {
        printf("%d\n", i);
        i += 1;
    }

}

int main() {
    int x = 20;
    struct Foo foo = {0};
    ZeusArray___ xs = (ZeusArray___){.items = (void*)ARRAY_DATA__824_844, .count = 3};
    take_array(xs);
    take_foo_ptr((&foo));
    take_foo(foo);
    take_int(foo.bar);
    printf("Counting up to %d\n", x);
    int i = 1;
    while ((i <= x)) {
        printf("%d, ", i);
        if (((i % 20) == 0)) {
            printf("\n");
        }

        i += 1;
    }

    return 0;
}

