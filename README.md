hetero-array: fast read-only heterogeneous data structures
==========================================================

This module is extracted from <http://hackage.haskell.org/package/web-routing web-routing>, orginally desgined for high performance type safe routing.

The basic idea is:

1. Construct a heterogeneous linked-list is O(n), since prepend is O(1).

2. Convert it into a heterogeneous array in O(n).

3. Following access will be a simple O(1) array indexing, with index computed at compile time so you can't get missing keys.

Typical usage: a heterogeneous lookup table, indexed by type level string.

Example
-------

```
> :set -XDataKinds -XQuasiQuotes
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
