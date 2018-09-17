//
// Created by Андрей on 20.05.2016.
//

#include "format.h"

namespace Format {

    std::string formatImplement (const std::string &fmt, unsigned pos, unsigned printed){
        return getSpeck (fmt, pos, false);
    }

    std::string getSpeck (const std::string &fmt, unsigned &pos, bool has_arguments){
        std::string result = "";

        while(pos < fmt.length ()){
            for(; pos < fmt.length () && fmt [pos] != '%'; result.push_back (fmt [pos ++]));
            if(pos == fmt.length ()){
                if(has_arguments){
                    throw std::invalid_argument ("Too many arguments for format");
                }

                return result;
            }

            if(pos == fmt.length () - 1){
                throw std::invalid_argument("Spurious trailing '%%' in format");
            }

            if(fmt [pos + 1] == '%'){
                result.push_back ('%');
                pos += 2;
            } else {
                pos ++;
                if(!has_arguments){
                    throw std::out_of_range("Not enough arguments for format");
                }

                break;
            }
        }

        return result;
    }

    std::string sequence (char c, unsigned n) {
        std::string result = "";
        for (unsigned i = 0; i < n; i++) {
            result.push_back(c);
        }

        return result;
    }

    std::string print_at (nullptr_t value){
        return "nullptr";
    }
}

#ifndef RELEASE // TESTS
#include <climits>

int main(){
    return 0;
}
#endif

