# Homework 1. Walk

### Task
Write a program that will perform a circumvention of file system.

As input arguments to a program given two paths to files: `input file` and `output file`.

In the first file (`input`) in each line there is a path to somewhere. 
It can be a file, folder or something other, but program should start from this path 
and recursively bypass all folders and files inside. 
For each file should be calculated a hash function [FNV](https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function)
and written to the second file (`output`) in format:

```
[hash value %08x] [path from the start]\n
```

In case of impossibility of calculation of hash must be note of format:

```
00000000 [path from the start]\n
```

### Limits

* Only limits of file system (path length, encoding, ...)

### Challenges

#### The number of given arguments can be more or less than 2
> Less than 2 arguments is unacceptable: program should notify about this and correctly stop the execution

> More then 2 arguments is acceptable, but program may notify that there are extra arguments

#### Input arguments (*one of both*) can be `null`
> Path to `input` or `output` file can't be `null` so 
> program should notify about this and correctly stop the execution

#### Size of file can be more then size of RAM
> Instance of class `File` doesn't load file to the RAM but gives an opportunities 
> to get real size of file throw the method `length ()` (*provided by file system*). 

```java
// Get available memory for the JVM
Runtime.getRuntime ().maxMemory ();
```

> To avoid overflow of RAM `RandomAccessFile` can be used with access methods `seek ()`.

#### Given path may route to non-existing file - Access to the file or directory can be restricted
> To check that given file exists you may use `file.exists ()` or `Files.exists (path)`.
> Also access to the file can be tried by methods `file.canRead ()`, `file.canWrite ()` or
> `Files.isReadable (path)`, `Files.isWriteable ()`.

> If it's `input` file then program should notify about this and correctly stop the execution

> If it's `output` file then program should try to create new file or notify about failure
> and correctly stop the execution

> If it's a path in `input` file then according to the conditions: `00000000` should be written as a hash

#### Given path may route to *not file* and to *not directory*
> To prevent this use methods `file.isFile ()` and `file.isDirecoty ()`.

> If it's a path in `input` file then according to the conditions: `00000000` should be written as a hash

#### Service symbols can be used in paths (such as `/`, `\`, `?`, `:`, ...)
> Given symbols can't be used in the name of folders or files, but they easily can be in given paths
> in `input` file of in given arguments to the program (f.e. `/home/te?st/fil:e`). 
> So program should notify about wrong path and skip such paths if it's possible.

#### Unicode symbols can be used in paths (f.e. Russian letters)
> All paths must be read with a `Reader` that supports Unicode (f.e. UTF-8), 
> because not only ASCII symbols (1 byte) can be used in paths but any other of Unicode (2 bytes).
> Use this variants to open `Reader` with declared encoding:

```java
// This is for java.io.* packet

try (
	InputStream is = new FileInputStream (file);
	Reader r = new InputStreamReader (is, Charset.forName ("UTF-8"));
	BufferedReader br = new BufferedReader (r); // It's optional
) {
	// Your code here
} catch (UnsupportedEncodingException uee) {
	// Wrong name of encoding
} catch (IOException ioe) {
	// Failed to read form file
}

// The same for Writer
```

```java
// This is for java.nio.* packet
try (
	// It uses UTF-8 by default, but see docs for more information
	Reader reader = Files.newBufferedReader (path);
) {
	// Your code here
} catch (IOException ioe) {
	// Failed to read form file
}

// The same for Writer
```

**Remark**: It is not excluded that other challenges can be.
This is mostly important and widespread (*in my opinion*).