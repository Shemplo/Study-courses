//
// Created by Андрей on 21.05.2016.
//

#ifndef TASK6_LAZY_STRING_H
#define TASK6_LAZY_STRING_H

#include <string>
#include <istream>
#include <ostream>
#include <memory>

/**
 * The lazyString class implements Copy-on-Write string
 */

class lazyString {
    struct charReferences {
        operator char () const;
        charReferences &operator = (char);

        friend class lazyString;

    private:
        const size_t index;
        lazyString *const lzString;

        charReferences (lazyString *, size_t);
    };

public:
    /**
     * Creates a std::string with a copy of the content of the current lazyString.
     *
     * @return  A std::string containing a copy of the characters of the current lazyString.
     */
    operator std::string();

    /**
     * Constructs empty lazyString (zero size and unspecified capacity).
     */
    lazyString ();

    /**
     * Constructs new lazyString from a std::string.
     *
     * @param   str A std::string.
     */
    lazyString (const std::string &str);

    /**
     * Returns the number of characters in the string
     *
     * @return The number of characters in the string.
     */
    size_t size () const;

    /**
     * Returns the number of characters in the string
     *
     * @return The number of characters in the string.
     */
    size_t length () const;

    /**
     * Returns a substring [pos, pos+count).
     * If the requested substring extends past the end of the string,
     * or if count == npos, the returned substring is [pos, size()).
     * Does not copy the buffer of the lazyString.
     *
     * @param   pos position of the first character to include
     * @param   len length of the substring
     *
     * @return  1) An empty string if pos == size().
     * @return  2) lazyString containing the substring [pos, pos+count).
     *
     * @throws  std::out_of_range if pos > size()
     */
    lazyString substr (size_t pos = 0, size_t length = std::string::npos);

    /**
     * Returns a reference to the character at specified location pos.
     * Bounds checking is performed, exception of type std::out_of_range
     * will be thrown on invalid access.
     *
     * @param   pos position of the character to return
     *
     * @return  Reference to the requested character.
     *
     * @throws  std::out_of_range if pos >= size().
     */
    charReferences at(size_t pos);
    char at(size_t pos) const;

    /**
     * Returns a reference to the character at specified location pos.
     * No bounds checking is performed.
     *
     * If pos == size(), a reference to the null character is returned.
     * For the first (non-const) version, the behavior is undefined if this character is modified.
     *
     * @param   pos position of the character to return
     *
     * @return  Reference to the requested character.
     *
     * @throws  std::out_of_range if pos >= size().
     */
    charReferences operator [] (size_t pos);
    char operator [] (size_t pos) const;

    /**
     * Extracts a string from the input stream is, storing the sequence in ls,
     * which is overwritten (the previous value of ls is replaced).
     *
     * @param   is  std::istream object from which characters are extracted.
     * @param   ls  lazyString object where the extracted content is stored.
     *
     * @return  The same as parameter is.
     */
    friend std::istream &operator >> (std::istream &is, lazyString &ls);

    /**
     * Inserts the sequence of characters that conforms value of str into os.
     *
     * @param   os  std::ostream object where characters are inserted.
     * @param   ls  lazyString object with the content to insert.
     *
     * @return  The same as parameter os.
     */
    friend std::ostream &operator << (std::ostream &os, lazyString &ls);

private:
    size_t head, stringSize;
    std::shared_ptr <std::string> ref;
    lazyString (size_t start, size_t sz, std::shared_ptr <std::string> ref);
};

#endif //TASK6_LAZY_STRING_H
