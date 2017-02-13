Power Set
=========

Having to iterate over a power set of some set has cropped up in toy problems
I've come across. Usually there is a more efficient way of solving the problem
but this "power set approach" is, for me, typically the easiest way to get a
handle on solving the problem. It is probably the "brute force" way to solve
these types of problems. The algorithm goes something like this:

- generate a power set, now the goal is to choose the "best" element of the
  power set so you...
- apply a function to each member of the power set
- choose the one which produced the "best" value

Code Examples
-------------

Here are some algorithms in haskell to produce the power set of a set. I've
chosen haskell because it is very succient and so should be simple to
translate to other languages if desired:

```
-- top down approach
powerSet1 :: [a] -> [[a]]
powerSet1 = foldr (\a b -> map (a:) b ++ b) [[]]

-- in case we forget how foldr works
powerSet2 :: [a] -> [[a]]
powerSet2 [] = [[]]
powerSet2 (x:xs) = let xsPowerSet = powerSet2 xs in map (x:) xsPowerSet ++ xsPowerSet

-- a bottom up approach
powerSet3 :: [a] -> [[a]]
powerSet3 = foldl (\b a -> map (a:) b ++ b) [[]]

-- in case we forget how foldl works
powerSet4 :: [a] -> [[a]]
powerSet4 s = go s [[]]
  where go [] acc = acc
        go (x:xs) acc = go xs (map (x:) acc ++ acc)
```
