-- https://lab.iisg.agh.edu.pl/fp/lab1.html
{--
  @author DH
--}
printHello = putStrLn "Hello"
main = printHello

sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt (x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (a, b) = (b, a)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x==y && y==z)

triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = sqrt
  ((a + b + c)*(a + b - c)*(a - b + c)*(-a + b + c))/4

sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
             then 0
             else 1

absInt :: Int -> Int
absInt n = if n >= 0
           then n
           else (-n)

min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a < b
                 then a
                 else b

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = if a < b && a < c
                    then a
                    else if b < a && b < c
                    then b
                    else c

min3Int_2 :: (Int, Int, Int) -> Int
min3Int_2 (a, b, c) = if min2Int(a, b) == min2Int(b, c)
                      then b
                      else if min2Int(a, c) == min2Int(c, b)
                      then c
                      else a


toUpper :: Char -> Char
toUpper sign = toEnum (if asciiCode < 97 || asciiCode > 122
                       then asciiCode
                       else asciiCode - 32)
                       where asciiCode = fromEnum sign

toLower :: Char -> Char
toLower sign = toEnum (if asciiCode < 65 || asciiCode > 90
                       then asciiCode
                       else asciiCode + 32)
                       where asciiCode = fromEnum sign

isDigit :: Char -> Bool
isDigit sign = asciiCode >= 48 && asciiCode <= 57
               where asciiCode = fromEnum sign

charToString :: Char -> String
charToString c = [c]

charToNum :: Char -> Int
charToNum sign = if isDigit sign
                then asciiCode - 48
                else error errorMsg
                where
                  asciiCode = fromEnum sign
                  strFromChar = charToString sign
                  errorMsg = "Error: '" ++
                             strFromChar ++
                             "' is not a digit"

toRomanDigit :: Char -> String
toRomanDigit sign = if isDigit sign && asciiCode /= 48
                    then romanList !! (arabicDigit - 1)
                    else
                      error errorMsg
                    where
                      asciiCode = fromEnum sign
                      strFromChar = charToString sign
                      romanList = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
                      arabicDigit = asciiCode - 48
                      errorMsg = "Error: '" ++
                                 strFromChar ++
                                 "' cannot conver to roman digit"

absInt_2 :: Int -> Int
absInt_2 n | n >= 0 = n
           | otherwise = -n

sgn_2 :: Int -> Int
sgn_2 n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1

min3Int_3 :: (Int, Int, Int) -> Int
min3Int_3 (a, b, c) | a < b && a < c = a
                    | b < a && b < c = b
                    | otherwise = c

not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _ = False

or' :: (Bool, Bool) -> Bool
or' (True, True) = True
or' (True, False) = True
or' (False, True) = True
or' (False, False) = False

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (True, False) = False
and' (False, True) = False
and' (False, False) = False

nand' :: (Bool, Bool) -> Bool
nand' (a, b) = not' (and' (a, b))

xor' :: (Bool, Bool) -> Bool
xor' (True, True) = False
xor' (True, False) = True
xor' (False, True) = True
xor' (False, False) = False

not'_2 :: Bool -> Bool
not'_2 b = case b of
           True -> False
           False -> True

absInt_3 :: Int -> Int
absInt_3 n =
  case (n >= 0) of
    True -> n
    _ -> -n

-- isItTheAnswer :: String -> Bool
-- not' :: Bool -> Bool
-- or' :: (Bool, Bool) -> Bool
-- and' :: (Bool, Bool) -> Bool
-- nand' :: (Bool, Bool) -> Bool
-- xor' :: (Bool, Bool) -> Bool

roots :: (Double, Double, Double) -> (Double,  Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e)
      where d = sqrt (b * b - 4 * a * c)
            e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = ( d * a, d * b)
          where d = 1 / sqrt (a * a + b * b)

roots_2 :: (Double, Double, Double) -> (Double,  Double)
roots_2 (a, b, c) =
        let d = sqrt (b * b - 4 * a * c)
            e = 2 * a
        in ( (-b - d) / e, (-b + d ) / e )
