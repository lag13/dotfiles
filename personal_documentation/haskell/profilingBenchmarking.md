Profiling, Microbenchmarking
============================

Profiling vs Benchmarking vs Microbenchmarking:
http://stackoverflow.com/questions/34801622/difference-between-benchmarking-and-profiling

The first answer here had good information:
http://stackoverflow.com/questions/15763050/haskell-measuring-function-performance.

Profiling
---------

Profiling is measuring the performance of your entire program with the goal of
identifying which parts take the longest to run. Once you have data about how
long various functions take to execute you can make an educated decision about
where optimization is most valuable:

```
ghc -prof -fprof-auto -rtsopts yourProgram.hs
./yourProgram +RTS -p
```

That will generate a `yourProgram.prof` file which you can analyze to get a
sense for how long everything is taking.

Microbenchmarking
------------------

Use the criterion package: http://hackage.haskell.org/package/criterion.

Microbenchmarking is measuring the speed of one specific function with the
goal of optimizing said function.

### Example

I was curious to see how much faster a powerSet function using `foldl'` would
be when compared to one using `foldr`. Turns out that for this particular
implementation, the difference was not so great! Note that I'm still not sure
whether I should be using the `nf` function or the `whnf` function. When using
the `whnf` the `foldr` implementation was faster and for `nf` the `foldl'`
implementation was moderately faster (but not enought to make a difference it
would appear):

```haskell
import Criterion.Main
import Data.List (foldl')

-- powerSet returns the power set of a set.
powerSet :: [Integer] -> [[Integer]]
powerSet = foldr (\a b -> map (a:) b ++ b) [[]]

-- powerSet' returns the power set of a set.
powerSet' :: [Integer] -> [[Integer]]
powerSet' = foldl' (\b a -> map (a:) b ++ b) [[]]

main = defaultMain [
  bgroup "powerSet" [ bench "[]"  $ nf powerSet []
               , bench "[1..4]"  $ nf powerSet [1..4]
               , bench "[1..8]"  $ nf powerSet [1..8]
               , bench "[1..16]" $ nf powerSet [1..16]
               , bench "[1..20]" $ nf powerSet [1..20]
               ],
  bgroup "powerSet'" [ bench "[]"  $ nf powerSet' []
               , bench "[1..4]"  $ nf powerSet' [1..4]
               , bench "[1..8]"  $ nf powerSet' [1..8]
               , bench "[1..16]" $ nf powerSet' [1..16]
               , bench "[1..20]" $ nf powerSet' [1..20]
               ]
  ]

```

I then compiled and ran:

```
ghc -O theFile.hs
./theFile --output powerSet.html
```

The output of that particular run was:

```
benchmarking powerSet/[]
time                 21.17 ns   (20.53 ns .. 21.85 ns)
                     0.992 R²   (0.987 R² .. 0.995 R²)
mean                 21.34 ns   (20.68 ns .. 22.17 ns)
std dev              2.493 ns   (2.001 ns .. 3.331 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking powerSet/[1..4]
time                 614.6 ns   (597.1 ns .. 636.3 ns)
                     0.988 R²   (0.979 R² .. 0.995 R²)
mean                 624.8 ns   (603.0 ns .. 653.3 ns)
std dev              86.44 ns   (62.03 ns .. 123.9 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking powerSet/[1..8]
time                 12.55 μs   (12.24 μs .. 12.92 μs)
                     0.994 R²   (0.992 R² .. 0.997 R²)
mean                 12.64 μs   (12.38 μs .. 12.92 μs)
std dev              849.2 ns   (696.6 ns .. 1.110 μs)
variance introduced by outliers: 73% (severely inflated)

benchmarking powerSet/[1..16]
time                 7.737 ms   (7.354 ms .. 8.150 ms)
                     0.983 R²   (0.975 R² .. 0.990 R²)
mean                 8.039 ms   (7.808 ms .. 8.269 ms)
std dev              675.5 μs   (559.7 μs .. 796.9 μs)
variance introduced by outliers: 48% (moderately inflated)

benchmarking powerSet/[1..20]
time                 190.7 ms   (170.3 ms .. 212.7 ms)
                     0.987 R²   (0.947 R² .. 0.999 R²)
mean                 186.0 ms   (177.7 ms .. 197.3 ms)
std dev              12.30 ms   (7.019 ms .. 16.72 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking powerSet'/[]
time                 21.92 ns   (21.11 ns .. 22.79 ns)
                     0.990 R²   (0.986 R² .. 0.994 R²)
mean                 22.58 ns   (21.96 ns .. 23.22 ns)
std dev              2.080 ns   (1.735 ns .. 2.691 ns)
variance introduced by outliers: 90% (severely inflated)

benchmarking powerSet'/[1..4]
time                 623.0 ns   (608.4 ns .. 644.6 ns)
                     0.992 R²   (0.989 R² .. 0.995 R²)
mean                 634.2 ns   (618.6 ns .. 654.2 ns)
std dev              60.98 ns   (50.96 ns .. 73.52 ns)
variance introduced by outliers: 89% (severely inflated)

benchmarking powerSet'/[1..8]
time                 12.55 μs   (12.17 μs .. 12.95 μs)
                     0.992 R²   (0.986 R² .. 0.996 R²)
mean                 12.94 μs   (12.57 μs .. 13.36 μs)
std dev              1.280 μs   (1.018 μs .. 1.563 μs)
variance introduced by outliers: 85% (severely inflated)

benchmarking powerSet'/[1..16]
time                 8.033 ms   (7.388 ms .. 8.701 ms)
                     0.949 R²   (0.892 R² .. 0.985 R²)
mean                 8.054 ms   (7.763 ms .. 8.824 ms)
std dev              1.279 ms   (605.6 μs .. 2.506 ms)
variance introduced by outliers: 78% (severely inflated)

benchmarking powerSet'/[1..20]
time                 179.2 ms   (167.6 ms .. 200.2 ms)
                     0.985 R²   (0.939 R² .. 0.998 R²)
mean                 175.6 ms   (166.0 ms .. 186.7 ms)
std dev              13.87 ms   (9.001 ms .. 20.67 ms)
variance introduced by outliers: 14% (moderately inflated)
```

The version using `whnf` had much faster times. It was an interesting insight
into the world of laziness and made me realize more that if a user doesn't
need the entire result of some computation then laziness might be nice.
