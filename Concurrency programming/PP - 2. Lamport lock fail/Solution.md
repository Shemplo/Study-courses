## 1. Некорректное исполнение
[![Build Status](https://travis-ci.com/ITMO-MPP-2017/lamport-lock-fail-Shemplo.svg?token=B2yLGFz6qwxKVjbLm9Ak&branch=master)](https://travis-ci.com/ITMO-MPP-2017/lamport-lock-fail-Shemplo)


## 2. Исправление алгоритма
```java
threadlocal int id       // 0..N-1 -- идентификатор потока
shared      int label[N] // заполненно нулями по умолчанию

def lock:
   1: label [id] = -1
   
   2: my = 1
   3: for k in range(N): if k != id:
   4:     my = max(my, label[k] + 1)
   5: label[id] = my
   6: for k in range(N): if k != id:
   7:     while true:
   8:         other = label[k]
   9:         if (other == 0 or (other, k) > (my, id)) and label [k] != -1: break@6

def unlock:
  10: label[id] = 0
```
