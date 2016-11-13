 {-# LANGUAGE BangPatterns #-}
import Data.List

myFun x = x * 2

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y

add2C :: Num a => a -> a -> a
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (a, b, c) = a + b + c

add3C :: Num a => a -> (a -> (a -> a))
add3C a b c = a + b + c

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = curry f

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f = uncurry f

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

-- uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
-- uncurry3 f =

fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^) -- fiveToPower_ 3 = 125


_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)-- _ToPower5 2 = 32

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -) -- subtrNFrom5 3 = 2

subtr5From_ :: Num a => a -> a
subtr5From_ = (-5 +) -- subtr5From_ 6 = 1

isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s -- isPalindrome "ABBA" = True

getElemAtIdx :: [Char] -> Int -> Char
getElemAtIdx s n = head (drop n s)

--capitalize :: [Char] -> [Char]
--capitalize w = ... -- capitalize "ala" = "Ala"

-- length [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2]

isPrime :: Integral t => t -> Bool
isPrime n | n == 1 = False
          | otherwise = [i | i <- [2..n-1], n `mod` i == 0] == []


-- length [x | x <- [1..10000], isPrime x == True]


primes :: [Int]
primes = eratoSieve [2..]
  where
    eratoSieve :: [Int] -> [Int]
    eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isInSetOf_:: [Int] -> Int -> Bool

isInSetOf_ listOfPrimes t = if t == (head listOfPrimes)
             then True
             else if t < (head listOfPrimes)
             then False
             else isInSetOf_ (drop 1 listOfPrimes) t
isInSetOfPrimes = isInSetOf_ primes

howManyPrimes :: Int -> Int
howManyPrimes n = length [x | x <- [1..n], isPrime x == True]


howManyPrimes2 :: Int -> Int
howManyPrimes2 n = length [x | x <- [1..n], isInSetOfPrimes x == True]


--allEqual :: Eq a => [a] -> Bool
--allEqual xs = [x | x <- xs, ] -- allEqual [1,1] = True, allEqual [1,2] = False

fib :: (Num a, Eq a) => a -> a
fib n =
  if n == 0 || n == 1 then n
  else fib (n - 2) + fib (n - 1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]
fib2 :: Int -> Int
fib2 n = fibs !! n

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' = sum'

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (_:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = False
and' [True] =  True
and' [False] = False
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' n [] = False
elem' n (x:xs) = n == x || elem' n xs

doubleAll :: Num t => [t] -> [t] -- double doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = [x * 2] ++ doubleAll xs

squareAll :: Num t => [t] -> [t] -- double squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = [x ^ 2] ++ squareAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven = loop [] where
  loop :: Integral t => [t] -> [t] -> [t]
  loop acc [] = acc
  loop acc (x:xs) = if x `mod` 2 == 0
                    then loop (acc ++ [x]) xs
                    else loop acc xs

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
  where loop acc []     = acc
        loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
  where loop acc []     = acc
        loop acc (x:xs) = loop (x + acc) xs

-- prod'2 :: Num a => [a] -> a

length'2 :: [a] -> Int
length'2 = loop 0 where
  loop acc [] = acc
  loop acc (x:xs) = loop (1 + acc) xs

sum'4 = loop 0
  where
    loop !acc []     = acc
    loop !acc (x:xs) = loop (x + acc) xs

isOdd :: (Ord a, Num a) => a -> Bool
isOdd n | n <= 0    = False
        | otherwise = isEven (n-1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n | n < 0     = False
         | n == 0    = True
         | otherwise = isOdd (n-1)

ackerFun m n
   | m == 0    = n + 1
   | n == 0    = ackerFun (m - 1) 1
   | otherwise = ackerFun (m - 1) (ackerFun m (n - 1))

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = [ y | y <- xs, y <= x ]
   rightPart xs = [ y | y <- xs, y > x  ]

qSort2 :: Ord a => [a] -> [a]
qSort2 []     = []
qSort2 (x:xs) = qSort2 (leftPart xs) ++ [x] ++ qSort2 (rightPart xs)
  where
    leftPart xs = filter (\y -> y <= x) xs
    rightPart xs = filter (\y -> y > x) xs

-- (opcjonalne) Zdefiniować funkcję mSort (implementacja algorytmu MergeSort)
-- (opcjonalne) Zdefiniować funkcję iSort (implementacja algorytmu Insertion Sort)

isSortedASC, isSortedDESC :: [Int] -> Bool
isSortedASC [] = True
isSortedASC [x] = True
isSortedASC (x:xs) = x <= head xs && isSortedASC xs

isSortedDESC [] = True
isSortedDESC [x] = True
isSortedDESC (x:xs) = x >= head xs && isSortedDESC xs

isSorted :: [Int] -> Bool -- isSorted [1,2,2,3] = True
isSorted xs = (isSortedASC xs) || (isSortedDESC xs)

reverse' :: [a] -> [a] -- reverse [1,2,3] = [3,2,1]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

-- zip' :: [a] -> [b] -> [(a,b)] -- zip' [1,2] [3,4] = [(1,3), (2,4)]
-- unzip' :: [(a, b)] -> ([a],[b]) -- unzip [(1,2), (3,4)] = ([1,3],[2,4])
-- zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
-- subList :: Eq a => [a] -> [a] -> Bool -- subList [1,2] [3,1,2,4] = True

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

isSecIsDivisibleByFst :: Integral a => [a] -> Bool
isSecIsDivisibleByFst (x : y : _) | (y `mod` x == 0) = True
isSecIsDivisibleByFst _                            = False
