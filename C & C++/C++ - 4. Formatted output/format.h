//
// Created by Андрей on 20.05.2016.
//

#ifndef FORMAT_FORMAT_H
#define FORMAT_FORMAT_H

#include <string>
#include <sstream>
#include <stdexcept>
#include <cstddef>
#include <iomanip>
#include <cstdio>
#include <typeinfo>
#include <stddef.h>

template <typename... Args> std::string format (const std::string& fmt, const Args&... args);

namespace Format {

    enum lengthType {
        lenHH,
        lenH,
        lenDefault,
        lenL,
        lenLL,
        lenJ,
        lenZ,
        lenT,
        lenBL,
        lenError
    };

    struct formatType {
        bool sign = false,
                leftJustify = false,
                operatorOrSpace = false,
                useOtherFormat = false,
                paddingLeft = false,
                uppercase = false,
                floating = false;

        int width = 0, precision = -1;

        char type;
        enum lengthType length = lenDefault;
    };

    template<typename To, typename From>
    typename std::enable_if<std::is_convertible<From, To>::value, To>
    ::type convert(From value) {
        return (To) value;
    }

    template<typename To, typename From>
    typename std::enable_if<!std::is_convertible<From, To>::value, To>
    ::type convert(From value) {
        throw std::invalid_argument("Invalid argument type");
    }

    std::string getSpeck (const std::string &fmt, unsigned &pos, bool has_arguments);
    std::string formatImplement (const std::string &fmt, unsigned pos, unsigned printed);
    std::string sequence (char c, unsigned n);
    std::string print_at (nullptr_t value);

    template <typename T>
        typename std::enable_if <!std::is_integral <T>::value
                                    && !std::is_convertible <T, std::string>::value
                                    && !std::is_pointer <T>::value, std::string>
            ::type print_at (const T& value){
        throw std::invalid_argument("Invalid argument type");
    }

    template <typename T>
        typename std::enable_if <std::is_integral <T>::value, std::string>
            ::type print_at (T value){
        return std::to_string (value);
    }

    template <typename T, int num>
        typename std::enable_if <!std::is_convertible <T*, std::string>::value, std::string>
            ::type print_at (const T (&a) [num]) {

        std::string r = "[";
        for(int i = 0; i < num - 1; i ++){
            r.append(std::to_string (a[i]) + ", ");
        }

        r.append(std::to_string (a [num - 1]) + ']');

        return r;
    }

    template <typename T>
        typename std::enable_if <std::is_convertible <T, std::string>::value, std::string>
            ::type print_at (const T& value){
        return value;
    }

    template <typename T>
        typename std::enable_if <!std::is_array <T>::value && !std::is_convertible <T, std::string>::value && std::is_pointer <T>::value, std::string>
            ::type print_at (T& value){

        std::string r;

        if(value == 0){
            r.append("nullptr<").append (typeid (*value).name ()).append (">");
        } else {
            r.append("ptr<").append (typeid (*value).name ()).append (">(").append (format ("%@", *value)).append (")");
        }

        return r;
    }

    template <typename T>
        typename std::enable_if <std::is_arithmetic <T>::value, std::string>
            ::type printNumber (formatType fm, T value){

        if (!fm.floating){
            if (fm.precision < 0){
                fm.precision = 1;
            } else if (fm.paddingLeft) {
                fm.paddingLeft = false;
            }
        }

        std::string temp = "%";
        if (fm.sign){temp.push_back ('+');}
        if (fm.leftJustify){temp.push_back ('-');}
        if (fm.operatorOrSpace){temp.push_back (' ');}
        if (fm.useOtherFormat){temp.push_back ('#');}
        if (fm.paddingLeft){temp.push_back ('0');}

        if (fm.precision >= 0){
            temp.push_back ('.');
            temp.append(std::to_string (fm.precision > 1024 ? 1024 : fm.precision));
        }

        char buffer[2048];
        if (fm.floating){
            if (fm.length == lenBL){temp.push_back ('L');}
            if (fm.length == lenL){temp.push_back ('l');}
            temp.push_back (fm.type);
        } else {
            temp.push_back ('j');
            temp.push_back (fm.type);
        }

        snprintf (buffer, sizeof (buffer), temp.c_str (), value);
        std::string r = buffer;

        if(fm.precision > 1024 && r.size () > 1024 / 2){
            if(fm.floating){
                r = r + sequence ('0', fm.precision - r.size () + r.find_first_of ('.') + 1);
            } else {
                r = r.substr (0, 2) + sequence ('0', fm.precision - r.size () + (r [0] == '0' ? 0 : 1)) + r.substr (2);
            }
        }

        if((unsigned) fm.width > r.size ()){
            if (fm.leftJustify){
                r = r + sequence (' ', fm.width - r.size());
            } else {
                if (fm.paddingLeft){
                    r = (r.find_first_of ("+- ") == 0) ? r [0] + sequence ('0', fm.width - r.size()) + r.substr(1) :
                        sequence ('0', fm.width - r.size ()) + r;
                } else {
                    r = sequence (' ', fm.width - r.size ()) + r;
                }
            }
        }

        return r;
    }

    template <typename First, typename... Rest>
        std::string formatImplement (const std::string& fmt,
                                 unsigned pos,
                                 unsigned printed,
                                 const First& value,
                                 const Rest&... args) {

        std::string result = getSpeck (fmt, pos, true);
        formatType fm;
        std::string temp = "";

        while(pos < fmt.length ()
                && (fmt [pos] == '-'
                    || fmt [pos] == '+'
                    || fmt [pos] == ' '
                    || fmt [pos] == '#'
                    || fmt [pos] == '0')){

            switch(fmt [pos ++]){
                case '-':
                    fm.leftJustify = true;
                    fm.paddingLeft = false;
                    break;
                case '+':
                    fm.sign = true;
                    fm.operatorOrSpace = false;
                    break;
                case ' ': fm.operatorOrSpace = !fm.sign; break;
                case '#': fm.useOtherFormat = true; break;
                case '0': fm.paddingLeft = !fm.leftJustify; break;
            }
        }

        if(pos < fmt.length() && fmt[pos] == '*'){
            fm.width = convert <int> (value);
            if(fm.width < 0){
                fm.width *= -1;
                fm.leftJustify = true;
                fm.paddingLeft = false;
            }

            temp = "%";
            if (fm.sign){temp.push_back ('+');}
            if (fm.leftJustify){temp.push_back ('-');}
            if (fm.operatorOrSpace){temp.push_back (' ');}
            if (fm.useOtherFormat){temp.push_back ('#');}
            if (fm.paddingLeft){temp.push_back ('0');}

            temp.append (std::to_string (fm.width));
            return result + Format::formatImplement (temp + fmt.substr (pos + 1, std::string::npos), 0, printed + result.length (), args...);
        } else {
            for(; pos < fmt.length () && isdigit (fmt [pos]); temp.push_back (fmt [pos ++]));
            if(!temp.empty()){
                fm.width = stoi (temp);
                temp.clear ();
            }
        }

        if(pos < fmt.length () - 1 && fmt [pos] == '.'){
            pos ++;

            if(fmt [pos] == '*'){
                fm.precision = convert <int> (value);
                temp = "%";

                if (fm.sign){temp.push_back ('+');}
                if (fm.leftJustify){temp.push_back ('-');}
                if (fm.operatorOrSpace){temp.push_back (' ');}
                if (fm.useOtherFormat){temp.push_back ('#');}
                if (fm.paddingLeft){temp.push_back ('0');}
                if (fm.width != 0){temp.append (std::to_string (fm.width));}

                temp.push_back ('.');
                temp.append (std::to_string (fm.precision));
                return result + formatImplement (temp + fmt.substr (pos + 1, std::string::npos), 0, printed + result.length (), args...);
            } else {
                if(fmt [pos] == '-'){
                    fm.precision = -1;
                    pos ++;
                } else {
                    fm.precision = 1;
                }

                for(; pos < fmt.length () && isdigit (fmt [pos]); temp.push_back (fmt [pos ++]));

                if(!temp.empty ()){
                    fm.precision *= stoi (temp);
                    temp.clear ();
                } else {
                    fm.precision = 0;
                }
            }
        }

        while (pos < fmt.length()
               && (fmt[pos] == 'h'
                   || fmt[pos] == 'l'
                   || fmt[pos] == 'j'
                   || fmt[pos] == 'z'
                   || fmt[pos] == 't'
                   || fmt[pos] == 'L')){

            switch(fmt [pos ++]){
                case 'h': fm.length = (fm.length == lenH) ? lenHH : ((fm.length == lenDefault) ? lenH : lenError); break;
                case 'l': fm.length = (fm.length == lenL) ? lenLL : ((fm.length == lenDefault) ? lenL : lenError); break;
                case 'j': fm.length = (fm.length == lenDefault) ? lenJ : lenError; break;
                case 'z': fm.length = (fm.length == lenDefault) ? lenZ : lenError; break;
                case 't': fm.length = (fm.length == lenDefault) ? lenT : lenError; break;
                case 'L': fm.length = (fm.length == lenDefault) ? lenBL : lenError; break;
            }
        }

        if(fm.length == lenError){
            throw std::invalid_argument("Unknown length specifier");
        }

        if(pos == fmt.length ()){
            throw std::invalid_argument("Lacks type at end of format");
        }

        std::stringstream out;
        if (fm.sign){ out << std::showpos; }
        if (fm.leftJustify){ out << std::left; }
        if (fm.width != 0){ out.width(fm.width); }
        if (fm.precision >= 0){ out.precision(fm.precision); }
        if (fm.useOtherFormat){ out << std::showbase << std::showpoint;

        intmax_t d;      // Integer
        uintmax_t u;     // Unsigned
        double f;        // Floating point
        char nils [6];   // Null pointer

        fm.type = fmt [pos ++];
        switch (fm.type){
            case 'd':
            case 'i':
                switch (fm.length){
                    case lenHH: d = convert <signed char> (value); break;
                    case lenH: d = convert <short int> (value); break;
                    case lenL: d = convert <long int> (value); break;
                    case lenLL: d = convert <long long int> (value); break;
                    case lenJ: d = convert <intmax_t> (value); break;
                    case lenZ: d = convert <size_t> (value); break;
                    case lenT: d = convert <ptrdiff_t> (value); break;
                    case lenDefault: d = convert <int> (value); break;
                    default:
                        throw std::invalid_argument("Unsupported length specifier");
                }

                result.append (printNumber (fm, d));
                break;
            case 'X':
                fm.uppercase = true;
            case 'x':
            case 'o':
            case 'u':
                switch (fm.length){
                    case lenHH: u = convert <unsigned char> (value); break;
                    case lenH: u = convert <unsigned short int> (value); break;
                    case lenL: u = convert <unsigned long int> (value); break;
                    case lenLL: u = convert <unsigned long long int> (value); break;
                    case lenJ: u = convert <uintmax_t> (value); break;
                    case lenZ: u = convert <size_t> (value); break;
                    case lenT: u = convert <ptrdiff_t> (value); break;
                    case lenDefault: u = convert <unsigned int> (value); break;
                    default:
                        throw std::invalid_argument("Unsupported length specifier");
                }

                result.append (printNumber (fm, u));
                break;
            case 'E': case 'G': case 'A':
                fm.uppercase = true;
            case 'e': case 'g': case 'a': case 'F': case 'f':
                fm.floating = true;
                switch (fm.length){
                    case lenL:
                    case lenDefault: f = convert <double> (value); break;
                    case lenBL: f = convert <long double> (value); break;
                    default:
                        throw std::invalid_argument("Unsupported length specifier");
                }

                result.append (printNumber (fm, f));
                break;
            case 'c':
                switch (fm.length){
                    case lenL: break;
                    case lenDefault: out << convert <unsigned char> (value); break;
                    default:
                        throw std::invalid_argument("Unsupported length specifier");
                }

                result.append (out.str ());
                break;
            case 's': {
                std::string str;
                switch (fm.length){
                    case lenL: break;
                    case lenDefault: str = convert <std::string> (value);
                        break;
                    default:
                        throw std::invalid_argument("Unsupported length specifier");
                }

                if(fm.precision >= 0 && str.length () > (unsigned) fm.precision){
                    str = str.substr (0, fm.precision);
                }

                out << str;
                result.append (out.str ());
            } break;

            case 'p':
                if(fm.length != lenDefault){
                    throw std::invalid_argument ("Unsupported length specifier");
                }

                out << std::setfill (fm.paddingLeft ? '0' : ' ');
                snprintf (nils, 2, "%p", convert <void*> (value));
                if(nils[0] != '(' && convert <void*> (value) != NULL && convert <void*> (value) != nullptr){
                    out << convert <void*> (value);
                } else {
                    out << "(nil)";
                }

                result.append (out.str ());
                break;
            case 'n':
                    printed += result.length ();
                switch (fm.length) {
                    case lenHH: *(convert <signed char*> (value)) = printed; break;
                    case lenH: *(convert <short int*> (value)) = printed; break;
                    case lenL: *(convert <long int*> (value)) = printed; break;
                    case lenLL: *(convert <long long int*> (value)) = printed; break;
                    case lenJ: *(convert <intmax_t*> (value)) = printed; break;
                    case lenZ: *(convert <size_t*> (value)) = printed; break;
                    case lenT: *(convert <ptrdiff_t*> (value)) = printed; break;
                    case lenDefault: *(convert <int*> (value)) = printed; break;
                    default:
                        throw std::invalid_argument("Unsupported length specifier");
                }

                break;
                case '@':
                    result.append (print_at (value));
                    break;
                default:
                    throw std::invalid_argument("Unknown format specifier: '" + fmt [pos] + '\'');
                    break;
            }

            return result + formatImplement (fmt, pos, printed + result.length (), args...);
        }
    }


    /**
     * Returns a std::string formatted with *printf syntax
     *
     * @param   fmt
     *          A <a href="http://cplusplus.com/printf">format string</a>
     *
     * @param   args
     *          Arguments required by the format specifiers in the format
     *          string. If there are more arguments than format specifiers, the
     *          extra arguments are ignored. The number of arguments is
     *          variable and may be zero.
     *
     * @throws  std::invalid_format
     *          If a format string contains an unexpected specifier,
     *          an argument can not be converted to required format,
     *          or in other illegal conditions.
     *
     * @throws  std::out_of_range
     *          If there are not enough arguments in args list
     *
     * @return  std::string, formatted using format and args
     */

    template <typename... Args>
        std::string format (const std::string& fmt, const Args&... args) {
        return Format::formatImplement(fmt, 0, 0, args...);
    }
}

#endif //FORMAT_FORMAT_H
