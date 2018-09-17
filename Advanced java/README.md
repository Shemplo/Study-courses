# Java advanced course etudes

### Organization of repository
Each folder with name like `Java - *. *` contains a task dedicated to a special topic of Java 
(it may contains several homework at the same folder). 

In folder (if skip common prefix) there are two folders `src` and `test`:

* In `src` folder placed source code that (possible: *with good chance*) passes review of *GA*.
* In `test` folder placed my own tests that can be used as **additional** challenge of **your** code. 
(*Remark: this tests are not alternative of GA's tests and have no power in protection of homework*)

#### Running of tests
Tests not require to be packed in a jar, 
just import `ru.shemplo.hw.test.*` package to your project and replace (if necessary) classes' names.

Run them as usual main class in IDE or with a commands:

``` 
javac -d bin -encoding UTF-8 -g:source -nowarn src
java -cp bin -Dfile.encoding=UTF-8 ru.shemplo.hw.test.*.*Test
```

### Terms of use
It's a public repository with a free access to a code. 
**But**, no guarantees that it works properly, 
no guarantees that it will pass review without modification,
no guarantees that it's the same code that was protected by author!

Also remind that cheating is not good and this repository only for familiarization purposes.
