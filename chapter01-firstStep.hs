-- importing ghci

-- 1.2 first function
doubleMe x = x + x

-- doubleUs x y = x * 2 + y * 2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- 1.3 リストの操作
-- head xs: リストの先頭要素を返す
-- tail xs: リストの先頭要素以外のリストを返す
-- last xs: リストの末尾要素を返す
-- init xs: リストの末尾以外のリストを返す
-- take n xs: 先頭からn個のリストを返す take 0 xsは[]が返る
-- drop n xs: 先頭からn個削除し, 残ったもののリストを返す
-- maximum, sum
-- product: リストの積を返す
-- x `elem` xs : xがxsに含まれているかを返す(Bool)
 
-- 1.4 range
-- [1..20]
-- ['a'..'z']
-- ['K'..'Z']

-- have step
-- [2,4..20]
-- [3,6..20]
-- ...etc

-- 1.5 リスト内包表記
-- 集合の内包表記に近い
-- {2*x | x ∈ N, x ≤ 10} みたいな
-- [x*2 | x <- [1..10]]
-- [出力 | 入力, 条件]
-- [x+y | x <- [10..20], y <- [10, 20, 30]] -- 全ての組み合わせをとる

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
removeOdd xss = [ [x | x <- xs, even x] | xs <- xss]


-- 1.6 Tuple
-- [(1,2), (0,4,5), (1,6)] エラー
-- => サイズ2のタプルとサイズ3のタプルの型は別の型である

-- ペアを使う
-- fst (8, 10) -> 8
-- snd (8, 10) -> 10
-- zip [1..5] ["i", "ii", "iii", "iv", "v"] -> [(1,"i"),(2,"ii"),(3,"iii"),(4,"iv"),(5,"v")]
-- zip [1..4] ["i", "ii", "iii", "iv", "v", "vi"] -> [(1,"i"),(2,"ii"),(3,"iii"),(4,"iv")]
-- 有限リストと無限リストをzipすることもできる(遅延評価の恩恵)

-- 直角三角形を見つける(整数のみ == 1:1:√2はない)
-- 被りあり
rightTriangles = [(a, b, c) | c <- [1..10], a <- [1..10], b <- [1..10], a^2 + b^2 == c^2]
-- 被りなし
rightTriangles' = [(a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]