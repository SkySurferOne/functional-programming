import Data.List
import Data.Char
--import Data.Set

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

sum''     = sumWith id
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

sortDesc, sortDesc' :: Ord a => [a] -> [a]
sortDesc' xs = (reverse . sort) xs
sortDesc = reverse . sort --  the same but in point-free notation

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
--are2FunsEqAt (+2) (\x -> x + 2) [1..1000] = True
are2FunsEqAt f g xs = map f xs == map g xs

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

-- złożenie listy funkcji
-- composeFunList :: [a -> a] -> (a -> a)
-- composeFunList ...
mindBlow = (.).(.) -- mind (^2) (\a b -> a^2 + b^2) 1 2 = 25

tuple = (((,) $ 1) $ 2)

onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
 | x `mod` 2 /= 0 = x : onlyOdd xs
 | otherwise      = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
 | x < 'a' = x : onlyUpper xs
 | otherwise = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = filter p

onlyEven'  = filter' (\x -> x `mod` 2 == 0)
onlyOdd'   = filter' (\x -> x `mod` 2 /= 0)
onlyUpper' = filter' (\x -> x < 'a')

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

map' :: (a -> b) -> [a] -> [b]
map' f = map f

doubleElems' = map' (\x -> x * 2)
sqrElems'    = map' (\x -> x^2)
--lowerCase   = map'

doubleElems'' xs = [x*2 | x <- xs]
sqrElems'' xs = [x^2 | x <- xs]

-- evalFuncListAt :: a -> [a -> b] -> [b]
-- evalFuncListAt x = map ...
-- wykorzystując map

sumWith' g []     = 0
sumWith' g (x:xs) = g x + sumWith' g xs -- (+) (g x) (sumWith g xs)

prodWith' g []     = 1
prodWith' g (x:xs) = g x * prodWith' g xs -- (*) (g x) (prodWith g xs)

sumWith'' :: Num a => (a -> a) -> [a] -> a
sumWith'' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith'' :: Num a => (a -> a) -> [a] -> a
prodWith'' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z = foldr f z

sumWith''' g  = foldr' (\x acc -> g x + acc) 0
prodWith''' g = foldr' (\x acc -> g x * acc) 1

{-
map wykorzystując foldr
map wykorzystując foldl
filter wykorzystując foldr
filter wykorzystując foldl
foldl wykorzystując foldr
foldr wykorzystując foldl
-}

iSortedAsc :: Ord a => [a] -> Bool
iSortedAsc xs = and $ zipWith (\a b -> a <= b) xs $ tail xs
-- isSortedAsc [1,2,2,3] -> True, isSortedAsc [1,2,1] -> False

-- everySecond :: [t] -> [t]
-- everySecond xs = ... -- everySecond [1..8] -> [1,3,5,7]

-- zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
-- zip3' ...
--
-- unzip3' :: [(a, b, c)] -> ([a], [b], [c])
-- unzip3' ...
--
-- iSortedDesc :: Ord a => [a] -> Bool
-- iSortedDesc xs ... -- isSortedDesc [3,2,2,1] -> True, isSortedDesc [1,2,3] -> False
--
-- iSorted :: Ord a => [a] -> Bool
-- iSorted xs ... -- isSorted [1,2,2,3] -> True, isSorted [3,2,1] -> True

-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

-- ones = 1 : ones
-- nats = 1 : map (+1) nats

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

-- concat ___ map (___) ___ [1..5] -- [2,4,6,8,10]
-- concatMap (___) [1..5] -- [2,4,6,8,10]
-- concatMap (___) ["Ready", "Steady", "Go"] -- "Ready!Steady!Go!"

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : (map toLower xs)

formatStr s = foldr1 (\w s -> w ++ " " ++ s) .
          map capitalize .
          filter (\x -> length x > 1) $
          words s

prodPrices p = case p of
 "A" -> 100
 "B" -> 500
 "C" -> 1000
 _   -> error "Unknown product"

products = ["A","B","C"]

-- basic discount strategy
discStr1 p
 | price > 999 = 0.3 * price
 | otherwise   = 0.1 * price
 where price = prodPrices p

-- flat discount strategy
discStr2 p = 0.2 * prodPrices p

totalDiscout discStr =
 foldl1 (+) .
 map discStr .
 filter (\p -> prodPrices p > 499)

 -- (opcjonalne) Poprawić (zrefaktoryzować) powyższy kod
 -- (aspekty: czytelności, ogólności, wydajności)
