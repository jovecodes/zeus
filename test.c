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
signed long bar ;
};

// ----------- FORWARD DECLARATIONS (FUNCTIONS) -----------
void take_foo_ptr(struct Foo* foo);
void take_foo(struct Foo foo);
void take_int(signed long a);
void take_array(ZeusArray___ arr);
signed int main();

// ----------- ARRAY DATA -----------
static const double ARRAY_DATA__1000_1024[] = {10.5, 20, 30, };

// ----------- PROGRAM CODE -----------
#include <stdio.h>
;
void take_foo_ptr(struct Foo* foo) {
    foo->bar += 10;
}

void take_foo(struct Foo foo) {
    printf("taken foo: %d\n", foo.bar);
}

void take_int(signed long a) {
    printf("a = %d\n", a);
}

void take_array(ZeusArray___ arr) {
    signed long i = 0;
    while ((i < arr.count)) {
        printf("%d: %f\n", i, (((double*) arr.items)[i]));
        i += 1;
    }

}

signed int main() {
    signed long x = 20;
    struct Foo foo = {0};
    ZeusArray___ xs = (ZeusArray___){.items = (void*)ARRAY_DATA__1000_1024, .count = 3};
    take_array(xs);
    take_foo_ptr((&foo));
    take_foo(foo);
    take_int(foo.bar);
    return 0;
}

