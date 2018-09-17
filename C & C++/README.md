# Repository for C language course

### First Problem - Multiplication table
How must it works:
  1. User enters number N;
  2. Program should generate a table of multiplication (it's important!);
  3. User enters the request of two formats:
    * `x1` `y1` `x2` `y2` - program should display a part of table as a rectangle with top-left and bottom-right corners
    * `0`                 - program should abort execution
  4. Before the aborting program should clear allocated memory;

Language: `C only`

### Second Problem - Phone book
How must it works:
  1. Once program get a path to directory where book will be placed;
  1. User enters comand of several formats:
    * `find <string>` - program should display data, if found, in such format: __`<id> <name> <phone number>`__
      * Input `<string>` can contain a part of name or a part of phone number in different formats like:
        * +1 (234) 56-78
        * +1-234-56-78
        * +1 234 56 78
        * ... and others
      * Displayed `<name>` must looks the same as before writing in book
      * Field `<id>` programmer can create like he/she wants
    * `create <name> <phone number>` - program should write in book new line with input data
    * `delete <id>` - program should delete from book line width such `<id>`
    * `change <id> number <phone number>` - program should change number to `<phone number>` in line width such `<id>`
    * `change <id> name <name>` - program should change name to `<name>` in line with such `<id>`
    * `exit` - program should abort execution (all data must be saved for the next execution)
  1. If command is invalid then program should inform about the error (and continue execution);
  1. Book data must be updated in storage after each user's command;
  1. Before the aborting program should clear allocated memory;

Language: `C only`

### Third problem - Rational numbers
How must it works:

  1. Create a class `rational` with given methods:
    * `getNum ()` - return a numerator of rational number
    * `getDenom ()` - return a denominator of rational number
  1. Class should overloads the standart operators: `+` `-` `*` `/`;
  1. Rational numbers should be saved inside as a numenator and denominator;
  1. Project should be build with using CMake;

Language: `C/C++`

### Fourth Problem - Formatted output
How must it works (requirements):

  1. Header file `format.h`;
  1. Library `libformat.so` or `libformat.a`;
  1. It must have a format as `sprintf ()` function;
  1. Variable of type `std::string` is treated the same as `char *`;
  1. In case of absence - throws the exception `std::outofrangechar∗ == nullptr, "<null>"`;
  1. If the literal format does not correspond to the type of the argument - throws the exception `std::invalidargument`;

Format:
  ```
  #include <string>
  #include <stdexcept>
  
  template <typename... Args>
  std::string format (std::string const & format, Args... args);
  ```

Language: `C/C++`
