-- 2.1 明示的な型宣言
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- 2.2 一般的なHaskellの型
-- Intは有界
-- Integerは有界ではない
factrial :: Integer -> Integer
factrial n = product [1..n]

-- Floatは単精度浮動小数点数
circumference :: Float -> Float
circumference r = 2 * pi * r

-- Doubleは倍精度浮動小数点数 Floatの二倍のビット数のリソース
circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- Boolは真理値 True | False
-- CharはUnicode文字

-- 2.3 型変数
-- 型変数は, 型安全を保ったまま関数を複数の肩に対して動作できるようにしてくれる
-- 他の言語のGenericsに似ている
-- 型変数を用いた関数は多相的関数と呼ばれる


-- 2.4 型クラス　初級講座
-- 型クラスは何らかの振る舞いを定義するインターフェース
-- ある型クラスのインスタンスである型はその型クラスが記述する振る舞いを実装する
-- => 具体的に言うと型クラスは関数の集まりを定める

-- Eq型クラス
-- Eqは等価性をテストできる型に使われる
-- Eqクラスが実装すべき関数は(==), (/=)

-- Ord型クラス
-- 何らかの順序をつけられる型のための型クラス
-- 実装すべきは >, <, >=, <=

-- Show型クラス
-- ある値はShow型クラスのインスタンスになっていれば文字列として表現できる
-- show X でXを文字列で表示する

-- Read型クラス
-- Showと対をなす型クラス
-- 文字列を受け取り, Readのインスタンスの型の値を返す
-- read "4" はエラーとなる ("4"はInt, Float, Doubleなど候補が複数ある)
-- 型注釈で対応 read "4" :: Int => 4, read "4" :: Float => 4.0など

-- Enum型クラス
-- 順番に並んだ型, つまり要素の値を列挙できる型
-- 値をレンジの中で使える
-- Enumクラスのインスタンスとしては(), Bool, Char, Ordering, Int, Integer等がある
-- ['a'..'e'] -> "abcde"
-- succ 'a' -> 'b'

-- Bounded型クラス
-- Bounded型クラスのインスタンスは上限と下限を持つ
-- それぞれminBoundとmaxBoundで調べられる
-- タプルの全ての要素がBoundedのインスタンスであればそのタプル自身もBoundedになる
-- 例: maxBound :: (Int, Bool, Char) -> (9223372036854775807, True, \1114111)

-- Num型クラス
-- このインスタンスは数のように振る舞う
-- あらゆる数もまたた総定数として表現されていてNum型クラスの任意のインスタンスとして振る舞う
-- 20 :: Int, 20 :: Integer, 20 :: Float, 20 :: Doubleが可能
-- :t (*) -> (*) :: (Num a) => a -> a -> a
-- つまり Int * Intは可能, Int * Integer は不可能

-- Floating型クラス
-- Float Doubleが含まれる(浮動小数点数)

-- Intergral型クラス
-- Int, Integerが含まれる(整数全体のみ)
-- fromIntegral :: (Num b, Integral a) => a -> b
-- これは整数からもっと一般的な数を返す



-- 型クラスは抽象的なインターフェースとして定義されているので1つの型はいくつもの型クラスのインスタンスになれる
-- 一つの型クラスはいくつもの型をインスタンスとしてもてる
-- Char型をインスタンスとする型クラスはいくつもある(Eq, Ord等) 
-- 型をある型クラスのインスタンスとするために一旦別のインスタンスにする必要があることがある. 
-- 例えばOrdクラスのインスタンスになるには先にEqクラスのインスタンスになる必要がある
-- => EqクラスのインスタンスであることはOrdクラスのインスタンスになるための”必要条件”である
