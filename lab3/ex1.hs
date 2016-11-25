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

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:[]) = f x
sumWith f (x:xs) = (f x) + sumWith f xs

sum     = sumWith id
sumSqr  = sumWith (\y -> y ^ 2)
sumCube = sumWith (\y -> y ^ 3)
sumAbs  = sumWith abs

listLength = sumWith (\y -> 1)

sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

factorial :: Double -> Double
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)

sumWith2 :: (Num a, Floating b) => (a -> b -> b) -> [a] -> b -> b
sumWith2 f [] x = 0
sumWith2 f (k:[]) x = f k x
sumWith2 f (k:ks) x = (f k x) + sumWith2 f ks x

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = sumWith2 (\k x -> x ^ k / factorial (fromIntegral k)) [0..n]

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = ff f h where ff f h x = (f (x + h) - f x) / h

funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

funcListExt :: [ Double -> Double ]
funcListExt = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x, \t -> sqrt (1 + t)]
