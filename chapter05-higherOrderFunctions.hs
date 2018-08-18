-- chpater5 高階関数

-- 5.1 curry化
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- compare 100は関数を返す. compare :: Int -> Int-> Ordering なので100と比較する関数を返すことになる
compareWithHandred :: Int -> Ordering
compareWithHandred = compare 100

-- セクション
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- divideByTen 200 は 200 / 10 と等価

-- セクションで`-`を使うときは注意が必要
-- (-4)はセクションの定義から考えると,引数から4を引く関数になると思われるが(-4)はマイナス4を表す
-- したがって引き算する関数を作りたいときはsubtract関数を使う (subtract 4)

-- 5.2 高階実演
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyTwice (+3) 10 = 16 (+3) ((+3) 10)

-- myZipWith
-- 型宣言: 1つ目 二つの引数を取り1つ返す関数, a型の値のリスト, b型のリスト, これは1つ目の引数である関数の引数がa型とb型であるから 
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip' :: (a -> b -> c) -> b -> a -> c
-- -- flip' f = g
--     -- where g x y = f y x
-- flip' f y x = f x y

-- 5.3 関数プログラマの道具箱
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' _ [] = []
-- filter' p (x:xs)
--     | p x       = x : filter' p xs
--     | otherwise = filter' p xs

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999])
    where p x = x `mod` 3829 == 0

-- コラッツ列
-- 任意の自然数から開始する
-- 数が1ならば, 終了
-- 数が偶数なら, 2で割る
-- 数が奇数なら3倍して1を足す
-- 新しい値でこのアルゴリズムを繰り返す

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

-- numLongChains :: Int
-- numLongChains = length (filter isLong (map chain [1..100]))
--     where isLong xs = length xs > 15

-- map関数に複数の引数を与える
-- let listOfFuns = map (*) [0..1]
-- listOFuns は [(*0), (*1), (*2), ..]
-- よってlistOfFuns !! 4 = (*4) であり
-- (listOfFuns !! 4) 5 = (*4) 5 = 20

-- 5.4 ラムダ式
-- numLongChainsのisLongをlambdaで書き換え
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- カリー化と部分適用の動作がよくわかっていないと必要のないところでラムダ式を使ってしまう
-- 以下は等価
-- map (+3) [1,6,3,2]
-- map (\x -> x + 3) [1,6,3,2]

flip' ::  (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

-- 5.5 畳み込み
-- 畳み込み(fold)を使うとデータ構造(ex. リスト)を単一の値にまとめることができる
-- foldlで左畳み込み
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- a: 畳み込み対象, b: アキュムレータの初期値

-- suml
suml :: (Num a) => [a] -> a
-- suml xs = foldl (\acc x -> acc + x) 0 xs
suml = foldl (+) 0
-- 0: アキュムレータ初期値, 引数がないが suml xs = foldl (+) 0 xsなのでOK (curry化の恩恵)

-- 計算過程
-- suml [1,2,3,4,5] 
-- foldl (+) 0 [1,2,3,4,5]
-- foldl (+) (0+1) [2,3,4,5]
-- foldl (+) (0+1+2) [3,4,5]
-- foldl (+) (0+1+2+3) [4,5]
-- foldl (+) (0+1+2+3+4) [5]
-- foldl (+) (0+1+2+3+4+5) []

-- foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- 右畳み込み
mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs

-- 計算過程
-- mapr (+3) [1,2,3] 
-- foldr (\x acc -> (+3) x: acc) [] [1,2,3]
-- foldr (\ 1 [] -> (+3) 1: []) [] [2,3]
-- foldr (\ 2 [4] -> (+3) 2: [4]) [4] [3]
-- foldr (\ 3 [4,5] -> (+3) 1: [4,5]) [4,5] []


-- 左畳み込み (++が遅いため普通は右畳み込みを使う)
mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- 右畳み込みは無限リストにも動作するが左畳み込みはダメ

-- elem関数
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- foldl1とfoldr1 初期値を明示的に与える必要がない
-- リストの先頭(末尾)の要素を初期アキュムレータとして使い, たたみこむ
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

-- いくつかの畳み込みの例
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
-- 引数が逆になることを利用
-- reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- 右畳み込みなのは++より:のほうが早いため
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x: acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-- 別の視点から見た畳み込み
-- リスト [3,4,5,6]
-- に対する右畳み込みは本質的には以下
-- f 3 (f 4 (f 5 (f 6 z)))
-- fを+, 初期アキュムレータを0とすると
-- 3 + (4 + (5 + (6 + 0)))
-- (+) 3 ((+) 4 ((+) 5 ((+) 6 0))) //前置記法版

-- 左畳み込み: 2引数関数g, アキュムレータz
-- g (g (g (g z 3) 4) 5) 6

-- 無限リストを畳み込む
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- and' (repeat Flase)

-- repeat FalseはFalseの無限リスト
-- False && (False && (False && (False && (False && (False &&) ... ))))

-- Falseの無限リストはこれの二つ目に合致するので初めの1こ目を評価してFalseを返す(2引数目を無視する)
-- (&&) :: Bool -> Bool -> Bool
-- True && x = x
-- False && _ = False

-- スキャン
-- scanl, scanrはfoldl, foldrと似ている
-- scanlとscanrはfoldの過程をリストで返す
-- *Main> scanl (+) 0 [1,2,3,4,5]
-- [0,1,3,6,10,15]
-- *Main> scanr (+) 0 [1,2,3,4,5]
-- [15,14,12,9,5,0]

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- 5.6 $
-- 評価の優先度を下げる → ()を減らせる
-- *Main> sum (filter (> 10) (map (*2) [2..10]))
-- 80
-- *Main> sum $ filter (> 10) (map (*2) [2..10])
-- 80
-- *Main> sum $ filter (> 10) $ map (*2) [2..10]
-- 80

-- 5.7 関数合成
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- 関数合成は右結合: f (g (z x)) = (f . g . z) x
-- *Main> map (\x -> negate (abs x)) [5,-3,-6,7,3]
-- [-5,-3,-6,-7,-3]
-- *Main> map (negate . abs) [5,-3,-6,7,3]
-- [-5,-3,-6,-7,-3]

-- *Main> map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]]
-- [-14,-15,-27]
-- *Main> map (negate . sum . tail) [[1..5], [3..6], [1..7]]
-- [-14,-15,-27]

-- 多引数関数の関数合成
-- *Main> sum (replicate 5 (max 6.7 8.8))
-- 44.0
-- *Main> (sum . replicate 5) (max 6.7 8.8)
-- 44.0
-- *Main> sum . replicate 5 $ max 6.7 8.8
-- 44.0

-- *Main> replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
-- [180,180]
-- *Main> replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]
-- [180,180]


-- ポイントフリースタイル
-- カリー化のおかげで引数を省略できる
sum' :: (Num a) => [a] -> a
-- 両辺のxsが一致している
-- sum' xs = foldl (+) 0 xs
sum' = foldl (+) 0

-- もう一つの例
-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- ポイントフリースタイルで描きたいのでカッコを除く
fn = ceiling . negate . tan . cos . max 50

-- 関数合成によって可読性が悪くなる時がある
-- letで小さな問題にした方がわかりやすい

oddSquareSum :: Integer
-- oddSquareSum = sum (takeWhile (<1000) (filter odd (map (^2) [1..])))
-- 書き換え
oddSquareSum = sum .takeWhile (<1000) . filter odd $ map (^2) [1..]
-- みやすい！