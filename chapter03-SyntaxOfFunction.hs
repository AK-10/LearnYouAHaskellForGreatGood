-- 3.1 パターンマッチ
lucky :: Int -> String
lucky 7 = "Lucky number seven!"
lucky x = "sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factrial :: Int -> Int
factrial 0 = 1
factrial n = n * factrial (n - 1)

-- non-exhaustive patterns
charName :: Char -> String
charName 'a' = "Alex"
charName 'b' = "Bob"
charName 'c' = "Cecil"
-- charName 'h'とかでエラー

-- タプルのパターンマッチ
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- triple の要素分解
first :: (a, b, c) -> a
first (x,_,_) = x

second :: (a, b, c) -> b
second (_,x,_) = x

third :: (a, b, c) -> c
third (_,_,x) = x

-- リストのパターンマッチとリスト内包表記
-- xsはペアのリスト
-- [a+b | (a,b) <- xs]
-- [x*100+3 | (x,3) <- xs]

-- 独自のhead head'を作る
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- asパターン
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- 3.2 guard

-- bmiTell :: Double -> String
-- bmiTell bmi
--     | bmi <= 18.5 = "You're underweight, you emo, you!"
--     | bmi <= 25.0 = "You're supposedly normal. \
--                     \ Pffft, I bet you're ugly!"
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise   = "You're a whale, congratulations!"

-- bmiTell :: Double -> Double -> String
-- bmiTell weight height
--     | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
--     | weight / height ^ 2 <= 25.0 = "You're supposedly normal. \
--                     \ Pffft, I bet you're ugly!"
--     | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise a


-- 3.3 where
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. \
                    \ Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2   
        --   skinny = 18.5
        --   normal = 25.0
        --   fat = 30.0
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = first
          (l:_) = last

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = wheight / height ^ 2 


-- 3.4 let it Be

-- let式は let `binding` in `expression`
-- let where の違いはletは式であるがwhereはそうでない

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * 2 r ^ 2
    in sideArea + 2 * topArea

-- letはローカルスコープに関数を作るのに使える
-- (;)で区切りに使える
-- whereは読みやすさで使う人が多い？
-- [let square x = x * x in (square 5, square 3, square 2)]
-- リスト内包表記でのlet
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
-- (w, h) <- xsはジェネレータと呼ばれる

-- 3.5 case文
-- case の構文
-- case `expression` of `pattern` -> `result`
--                      `pattern` -> `result`
--                      `pattern` -> `result`

describeList :: [a] -> String
describeList ls = "The list is "
                  ++ case ls of [] -> "empty."
                                [x] -> "a singleton list."
                                xs -> "a longer list."


-- 次のようにも書ける
describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

