// translated program test
#include <stdio.h>

int a, b, c;
char * s;

int main () {
    s = "Hello_world";
    printf ("%s\n", s);

    s = "Enter_two_numbers:";
    printf ("%s\n", s);

    scanf ("%d %d", &a, &b);

    a = a * 2;
    b = (a * 2) * (a + 3);
    if (a > b) {
        if (b < 10) {
        a = b;
    } else {
        if (b > 10) {
        a = b / 2;
} else {
        a = 0;
    }
    }
}

    for (c = (b + 1) * 2; c >= 0; c --) {
        a = a + c;
    while (a > 2) {
    a = a - 1;
}

    }
    s = "Result:";
    printf ("%s %d %d %d\n", s, a, b, c);

    a = 10;
    b = 3;
    s = "div_and_mod_test";
    printf ("%s %d %d\n", s, a, b);

    c = a / b;
    s = "div_a";
    printf ("%s %d\n", s, c);

    c = a % b;
    s = "mod_a";
    printf ("%s %d\n", s, c);

    a = 10;
    do {
    printf ("%d\n", a);

    a = a - 1;

} while (!(a >= 0));

    return 0;
}

