#include <cmath>
#include "rational.h"

rational::rational (int up) {
    this->up = up;
    this->down = 1;
}

rational::rational (int up, int down) {
    this->up = up / gcd (up, down);
    this->down = down / gcd (up, down);
}

int rational::getNum () const {
    return this->up;
}

int rational::getDenom () const {
    return this->down;
}

int rational::gcd (int a, int b) const {
    int d = 0;
    a = std::abs (a);
    b = std::abs (b);

    if (b > a) {
        d = b;
        b = a;
        a = d;
    }

    while (a % b != 0) {
        d = a % b;
        a = b;
        b = d;
    }

    return b;
}

const rational rational::operator + (const rational &pointer) const {
    int newUp = this->up * pointer.down + pointer.up * this->down;
    int newDown = this->down * pointer.down;

    return rational (newUp, newDown);
}

const rational rational::operator - (const rational &pointer) const {
    int newUp = this->up * pointer.down - pointer.up * this->down;
    int newDown = this->down * pointer.down;

    return rational (newUp, newDown);
}

const rational rational::operator * (const rational &pointer) const {
    int newUp = this->up * pointer.up;
    int newDown = this->down * pointer.down;

    return rational (newUp, newDown);
}

const rational rational::operator / (const rational &pointer) const {
    int newUp = this->up * pointer.down;
    int newDown = this->down * pointer.up;

    return rational (newUp, newDown);
}
