module Toolbox where


linspace :: (Floating f, Enum f) => f -> (f, f) -> [f]
linspace n (from, to) = let interval = to - from
                            space = interval / (n-1)
                        in  [from, from+space..to]

bisect :: (Ord a, Enum n) => a -> [a] -> n -> Maybe n
bisect n xs s = bisect' n xs s
    where
        bisect' :: (Ord a, Enum n) => a -> [a] -> n -> Maybe n
        bisect' _ [] _ = Nothing
        bisect' _ [x] _ = Nothing
        bisect' n (x:y:xs) i | n >= x && n <= y = Just i
                             | otherwise = bisect' n (y:xs) (succ i)

for :: [a] -> (a -> b) -> [b]
for = flip map

apply :: a -> [(a -> a)] -> a
apply a [] = a
apply a (f:fs) = apply (f a) fs