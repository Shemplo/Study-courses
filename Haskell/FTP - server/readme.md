## FTP Server & Client on Haskell
> Server & client are compatibile with [RFC 959](https://tools.ietf.org/html/rfc959)

### Opportunities

**Notice!** there is no full support of mentioned _RFC_. Implemented only part of all command words and strategies. 
But they allow to compute basic requirements for _FTP_: navigation in remote file system, transporting files between hosts.

For client avalilable commands:
* **open [_host_ [_port_]]**  - open connection to specified _host_ by given _port_
> <pre>open 127.0.0.1 21 - example when all parameters are mentioned </pre>
> <pre>open 127.0.0.1    - in this case 21 port would be used</pre>
> <pre>open              - 127.0.0.1 host address and 21 port would be used</pre>

* **close** - close connection to the server (if connected)
> <pre>close             - just type this :)</pre>

* **exit** - terminate client application (also call **close** before execution)
> <pre>exit              - quite simple too</pre>

* **login _login_ _password_** - try to authorize connection by given _login_ and _password_
> <pre>login root 123    - would attempt to authorize under root user</pre>

* **cd _path_** - changes remote directory to resolved by _current path_ and given _path_ (no whitespaces allowed :sweat_smile:)
> <pre>cd ../next        - would rise to parent of current directory and then goes into `next` directory</pre>

* **lcd _path_** - do the same as **cd** but in local file system
> <pre>lcd after/..      - would goes into `after` directory and then goes back</pre>

* **list** - shows all directories and files in current directory on remote machine
> <pre>list              - nothing more needed</pre>

* **upload _filename_** - try to upload file with _filename_ from local machine to remote
> <pre>upload test.txt   - would send file `test.txt` from current directory on local machine to 
>                     `test.txt` in current directory on remote machine</pre>

* **delete _filename_** - try to delete file with _filename_ on remote machine (if exists)
> <pre>delete list.lst   - would delete `list.lst` on remote machine in current directory</pre>

* **download _filename_** - opposite to **upload _filename_**
> <pre>download test.txt - would send file `test.txt` to local machine (if exists)</pre>

### Example

Imagine that we want send to remote host file `/haskell/homeworks/Done.hs` to `/assigned/perfect/Done.hs`:
```
open 43.197.32.122 21
login root 123
cd assigned/perfect
lcd haskell/homeworks
upload Done.hs
list
exit
```

### Building server and client

This application is formed as `library` becase I didn't want to create separated projects :hugs:

`sudo stack --allow-different-user ghci`  

**Run server**: RunServer.main  
**Run client**: RunClient.main  

### Some notes

* Server will be always runned at _21_ port
* In most cases error in client will require restart of it
* On server available authorization only for `root/123`
* Only shared folder `ftp` for server is available (impossible to go upper it)
