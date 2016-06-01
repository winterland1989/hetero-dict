hetero-dict: fast heterogeneous data structures
===============================================

[![Travis-CI](https://travis-ci.org/winterland1989/hetero-dict.svg)](https://travis-ci.org/winterland1989/hetero-dict)

This package provide two flavor fast heterogeneous data structures:

1. `Dict` which use boxed array, it's read-only with O(1) get.

1. `DynDict` which use `Seq` from `Data.Sequence`, it has O(log(min(i,n-i))) get, modify and O(1) add.

Example
-------

```haskell
> :set -XDataKinds -XQuasiQuotes
> :m + Data.Hetero.Dict
> let d = mkDict . add [key|foo|] 12 . add [key|bar|] "baz" $ emptyStore
> :t d
d :: Num v => Dict '["foo" ':= v, "bar" ':= [Char]]
> get [key|foo|] d
12
> get [key|bar|] d
"baz"
> get [key|qux|] d
 • Couldn't match type ‘'Index i1’ with ‘'NotFoundKey "qux"’
 ...
```

```haskell
> :set -XDataKinds -XQuasiQuotes
> :m + Data.Hetero.DynDict
> let d =  add [key|foo|] 12 . add [key|bar|] "baz" $ empty
> get [key|foo|] d
12
> get [key|bar|] d
"baz"
> let d' = set [key|foo] 13 d
> get [key|foo|] d'
13
```
