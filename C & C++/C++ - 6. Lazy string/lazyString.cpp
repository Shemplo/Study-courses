//
// Created by Андрей on 21.05.2016.
//

#include <iostream>
#include "lazyString.h"

using namespace std;

lazyString::operator std::string() {
    return ref->substr(head, stringSize);
};

lazyString::lazyString(const std::string &str) : ref(std::make_shared <std::string> (str)), head(0), stringSize(str.size()) { }

lazyString::lazyString() : ref(std::make_shared<std::string> ("")), head(0), stringSize(0) { }

lazyString::lazyString(size_t start, size_t sz, std::shared_ptr<std::string> ref) : ref(ref), head(start), stringSize(sz) { }


size_t lazyString::size() const {
    return stringSize;
}

size_t lazyString::length() const {
    return stringSize;
}


lazyString lazyString::substr(size_t pos, size_t length) {
    if (pos > stringSize)
        throw std::out_of_range ("lazyString");
    return lazyString(head + pos, pos + length > stringSize ? (stringSize - pos) : length, ref);
}

lazyString::charReferences lazyString::at(size_t i) {
    if (i >= stringSize)
        throw std::out_of_range("lazyString");
    return charReferences(const_cast <lazyString *> (this), i);
}

lazyString::charReferences lazyString::operator [] (size_t i) {
    return charReferences(const_cast <lazyString *> (this), i);
}

char lazyString::at(size_t i) const {
    if (i >= stringSize)
        throw std::out_of_range ("lazyString");
    return (*ref)[head + i];
}

char lazyString::operator [] (size_t i) const {
    return (*ref)[head + i];
}

std::istream &operator>>(std::istream &input, lazyString &lString) {
    auto ref = std::make_shared <std::string> ();

    input >> *ref;
    lString.ref = ref;

    lString.head = 0;
    lString.stringSize = ref-> size ();

    return input;
}

std::ostream &operator << (std::ostream &output, lazyString &lString) {
    for (int i = 0; i < lString.stringSize; i ++)
        output << (*lString.ref) [lString.head + i];
    return output;
}


lazyString::charReferences::charReferences (lazyString *lString, size_t index) : lzString(lString), index(index) { }

lazyString::charReferences &lazyString::charReferences::operator = (char c) {
    if (lzString->ref.use_count () > 1) {
        lzString->ref =
                std::make_shared <std::string> (lzString->ref->substr (lzString->head,
                                                                       lzString->stringSize));
        lzString->head = 0;
    }
    (*lzString->ref) [lzString->head + index] = c;

    return *this;
}

lazyString::charReferences::operator char () const {
    return (*lzString->ref) [lzString->head + index];
}


