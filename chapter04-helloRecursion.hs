-- 4.1 最高に最高！
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- 4.2 さらにいくつかの再帰関数
-- 値を指定された回数だけ繰り返したリストを返す
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _ 
    | n <= 0 = []

take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' x:xs = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x: repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

-- 4.3 quick sort!
-- ソートしたいリスト [5,1,9,4,6,7,3]
-- [1,4,3] ++ [5] ++ [9,6,7]
-- [] ++ [1] ++ [4,3] ++ [5] ++ [6,7] ++ [9] ++ []
-- [] ++ [1] ++ [3] ++ [4] ++ [] ++ [5] ++ [] ++[6] ++ [7] ++ []

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a >= x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger


-- test
