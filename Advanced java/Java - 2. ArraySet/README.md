# Homework 2. ArraySet

### Task

Create class `ArraySet` that implements immutable sorted set.
The functional interface must be not less than `NavigableSet` has 
(except methods `pollFirst ()`, `pollLast ()`, ...).

Also asymptotic of all method should be well optimized.

### Limits

* Any class can be stored in this set
* Set won't be modified and only *read* methods will be called

### Challenges

#### How to compare to Objects
> **Equivalent**: each class in Java extended class `Object` 
that's why each instance has method `equals ()`. Also there
are method `Objects.equals ()` and `Objects.deepEquals ()`
for comparing two objects without checking objects on `null`.  
P.S. if it's a primitive type then operator `==` can be used for
this purposes 
(for objects this operator will compare just a pointers to the instances).

> **Attitude**: to determine order attitude on objects in Java there is
a special interface `Comparable`. 
It has method `compareTo ()` that by the contract can return: 
`1 == BIGGER`, `0 == EQUAL`, `-1 == LESS`.
For collections in Java there is interface `Comparator` that allows
to define order attitude for elements in such collection (if type of
elements in collection can be compared by default (they are `Comparable`)
then will be called method `compareTo ()` on the one of them).

> Problems is in the fact that not all classes implements such interface, and
by contract Java defines them as *EQUAL* (`0` attitude).

#### Getting rid of boilerplate code
> *In computer programming, boilerplate code or boilerplate refers to sections of code* 
*that have to be included in many places with little or no alteration*
([wiki](https://en.wikipedia.org/wiki/Boilerplate_code)).
To prevent this in general in Java can be used abstraction 
(using of interfaces and abstract classes/methods), in this task
may help using of standard collections (for example `ArrayList`).
Also issuance of common parts of methods to a separate method is
good practice too.

#### Getting rid of *@SuppressWarnings ("unchecked")*
> This problem is connected with *generic* types in Java. The reason
is that there is no polymorphism in generic types, and if class has
`<?>` generic type then it can't be safely casted to the same class
with specified generic type (even if it's guaranteed by *God*). To avoid
from this problem there is two ways: organize polymorphism on `<?>` generic type
with key words (`extends` and `super`) **or** re-organize code logic.

#### Navigation inside the set
> To improve performance (and asymptotic) of methods input sequence of data
can be sorted with standard methods at the beginning and then iterate by
using *binary search* from `Collections.binarySearch ()`. To sort
data from any `Collection` you can do this with `TreeSet` with specified comparator.

**Remark**: It is not excluded that other challenges can be.
This is mostly important and widespread (*in my opinion*).