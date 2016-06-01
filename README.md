hetero-dict: fast heterogeneous data structures
===============================================

[![Hackage](https://img.shields.io/hackage/v/hetero-dict.svg?style=flat)](http://hackage.haskell.org/package/hetero-dict)
[![Travis-CI](https://travis-ci.org/winterland1989/hetero-dict.svg)](https://travis-ci.org/winterland1989/hetero-dict)

This package provide two flavor fast and easy to use heterogeneous data structures:

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

Benchmark
---------

We use [hvect](http://hackage.haskell.org/package/hvect) package as a linked-list based reference.

```
benchmarking n = 3/Build Dict
time                 11.78 ns   (11.63 ns .. 11.93 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 11.72 ns   (11.62 ns .. 11.82 ns)
std dev              336.9 ps   (282.4 ps .. 406.1 ps)
variance introduced by outliers: 48% (moderately inflated)

benchmarking n = 3/Build DynDict
time                 18.10 ns   (17.96 ns .. 18.24 ns)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 18.42 ns   (18.10 ns .. 19.84 ns)
std dev              1.676 ns   (550.3 ps .. 3.725 ns)
variance introduced by outliers: 90% (severely inflated)

benchmarking n = 3/Build HVect
time                 16.39 ns   (15.90 ns .. 17.05 ns)
                     0.994 R²   (0.990 R² .. 0.998 R²)
mean                 16.72 ns   (16.34 ns .. 17.31 ns)
std dev              1.686 ns   (1.254 ns .. 2.193 ns)
variance introduced by outliers: 92% (severely inflated)

benchmarking n = 3/Index Dict
time                 56.35 ns   (54.47 ns .. 58.42 ns)
                     0.990 R²   (0.986 R² .. 0.995 R²)
mean                 55.85 ns   (54.31 ns .. 57.90 ns)
std dev              5.972 ns   (4.253 ns .. 7.946 ns)
variance introduced by outliers: 92% (severely inflated)

benchmarking n = 3/Index DynDict
time                 72.03 ns   (70.14 ns .. 74.63 ns)
                     0.989 R²   (0.980 R² .. 0.995 R²)
mean                 75.49 ns   (73.19 ns .. 78.75 ns)
std dev              9.245 ns   (7.589 ns .. 11.76 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking n = 3/Index HVect
time                 69.21 ns   (67.27 ns .. 71.86 ns)
                     0.994 R²   (0.989 R² .. 0.999 R²)
mean                 68.69 ns   (67.69 ns .. 70.13 ns)
std dev              4.080 ns   (2.918 ns .. 5.949 ns)
variance introduced by outliers: 78% (severely inflated)

benchmarking n = 15/Build Dict
time                 10.80 ns   (10.65 ns .. 10.97 ns)
                     0.997 R²   (0.995 R² .. 1.000 R²)
mean                 10.85 ns   (10.71 ns .. 11.15 ns)
std dev              628.9 ps   (344.3 ps .. 1.078 ns)
variance introduced by outliers: 79% (severely inflated)

benchmarking n = 15/Build DynDict
time                 37.11 ns   (36.55 ns .. 37.94 ns)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 37.77 ns   (37.04 ns .. 38.72 ns)
std dev              2.827 ns   (2.096 ns .. 3.682 ns)
variance introduced by outliers: 86% (severely inflated)

benchmarking n = 15/Build HVect
time                 15.82 ns   (15.23 ns .. 16.59 ns)
                     0.991 R²   (0.985 R² .. 0.999 R²)
mean                 15.61 ns   (15.31 ns .. 16.07 ns)
std dev              1.221 ns   (803.7 ps .. 1.757 ns)
variance introduced by outliers: 87% (severely inflated)

benchmarking n = 15/Index Dict
time                 281.6 ns   (279.6 ns .. 283.8 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 281.7 ns   (279.9 ns .. 283.9 ns)
std dev              6.878 ns   (5.580 ns .. 8.830 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarking n = 15/Index DynDict
time                 659.2 ns   (652.3 ns .. 665.6 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 662.4 ns   (656.8 ns .. 669.8 ns)
std dev              22.28 ns   (17.73 ns .. 30.08 ns)
variance introduced by outliers: 48% (moderately inflated)

benchmarking n = 15/Index HVect
time                 693.4 ns   (687.0 ns .. 698.7 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 690.7 ns   (683.2 ns .. 695.8 ns)
std dev              20.66 ns   (16.34 ns .. 29.80 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking n = 15/Modify DynDict
time                 98.74 ns   (97.12 ns .. 100.8 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 98.60 ns   (97.18 ns .. 100.1 ns)
std dev              5.091 ns   (3.999 ns .. 6.935 ns)
variance introduced by outliers: 72% (severely inflated)
```
