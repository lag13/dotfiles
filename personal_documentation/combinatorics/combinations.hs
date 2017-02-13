-- -- Perform any k-combination: http://stackoverflow.com/a/22577148:
-- combinations :: Int -> [a] -> [[a]]
-- combinations k xs = go (length xs) k xs
--   where go n k' l@(y:ys)
--           | k' == 0   = [[]]
--           | k' >= n   = [l]
--           | null l    = []
--           | otherwise = map (y:) (go (n-1) (k'-1) ys) ++ go (n-1) k' ys

-- Just for fun, what a 2-combination looks like
combinationsTake2 :: [a] -> [[a]]
combinationsTake2 [] = []
combinationsTake2 [_] = []
combinationsTake2 (x:xs) = map (:[x]) xs ++ combinationsTake2 xs

-- Perform any k-combination, modified from:
-- http://stackoverflow.com/a/22577148.
-- combinations :: Int -> [a] -> [[a]]
-- combinations 0 _ = [[]]
-- -- this case never gets hit when recursing, it would only get hit initially.
-- combinations _ [] = []
-- combinations k l@(x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

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
