f1 = (\x -> x - 1) :: Double -> Double
f2 = (\x y -> sqrt (x ^ 2 + y ^ 2)) :: Double -> Double -> Double
f3 = (\x y z -> sqrt (x ^ 2 + y ^ 2 + z ^ 2)) :: Double -> Double -> Double -> Double

multi2_ = \x -> 2 * x
_multi2 = \x -> x * 2
pow2_ = \x -> 2 ^ x
_pow2 = \x -> x ^ 2
div2_ = \x -> 2 / x
_div4 = \x -> x / 3
substr = \x -> 4 - x


f7 x = if x `mod` 2 == 0 then True else False
f7a = \x -> if x `mod` 2 == 0 then True else False

f8 x = let y = sqrt x in 2 * y^3 * (y + 1)
f8a = \x -> let y = sqrt x in 2 * y^3 * (y + 1)

f9 1 = 3
f9 _ = 0
f9a = \x -> if x == 1 then 3 else 0

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x ^ 2 + sumSqr' xs
