// ----------- INCLUDES -----------
#include <stdio.h>
#include <stdlib.h>

// ----------- FORWARD DECLARATIONS -----------
int main();

// ----------- PROGRAM CODE -----------
int main() {
    int x = 20;
    printf("Pick a number: \n");
    scanf("%d", (&x));
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

