#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main (int argc, const char *args []) {

    int n;
    scanf ("%d", &n);

    int **mass = (int **) malloc (n * sizeof (int *));
    for (int i = 0; i < n; i ++) {
        mass [i] = (int *) malloc (n * sizeof (int));

        for (int j = 0; j < n; j ++) {
            mass [i][j] = (i + 1) * (j + 1);
        }
    }

    int x1, y1;
    int x2, y2;

    while (true) {
        scanf ("%d", &x1);

        if (x1 == 0) {
            break;
        } else {
            scanf ("%d %d %d", &y1, &x2, &y2);

            //printf ("Debug set %d-%d and %d-%d\n", x1, y1, x2, y2);

            for (int i = 0; i < y2 - y1 + 1; i ++) {
                for (int j = 0; j < x2 - x1 + 1; j ++) {
                    printf ("%d ", mass [y1 + i - 1][x1 + j - 1]);
                }

                printf ("\n");
            }
        }
    }

    for (int i = n - 1; i >= 0; i --) {
        free (mass [i]);
    }

    free (mass);

    return 0;

}
