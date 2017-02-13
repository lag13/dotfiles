Combinations
============

Code
----

Here are some algorithms in haskell to produce combinations. I've chosen
haskell because it is very succient and so should be simple to translate to
other languages if desired:

```
-- Perform any k-combination, modified from:
-- http://stackoverflow.com/a/22577148. I tried to account for all possible
-- input scenarios.
combinations :: Int -> [a] -> [[a]]
combinations k l
  | k < 0 = error "it does not make sense to have a negative k"
  | otherwise = go k l
  where go 0 _ = [[]]
        go k l
          | k > length l = []
          | otherwise = map (head l:) (go (k-1) (tail l)) ++ go k (tail l)

-- Just for fun, what a 2-combination algorithm looks like.
combinationsTake2 :: [a] -> [[a]]
combinationsTake2 [] = []
combinationsTake2 [_] = []
combinationsTake2 (x:xs) = map (:[x]) xs ++ combinationsTake2 xs
```

