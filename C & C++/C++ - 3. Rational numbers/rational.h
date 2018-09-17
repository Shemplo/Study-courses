#pragma once

class rational {

    int up, down;

    public: rational (int up);
    public: rational (int up, int down);

    public: int getNum () const;
    public: int getDenom () const;

    private: int gcd (int, int) const;

    public: const rational operator + (const rational &pointer) const;
    public: const rational operator - (const rational &pointer) const;
    public: const rational operator * (const rational &pointer) const;
    public: const rational operator / (const rational &pointer) const;

};
