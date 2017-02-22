-- type, newType
-- rekursja strukturalna
-- rekordy
-- algebraiczne str danych
-- typy wyszego rzedu -- nie bedzie na kart

polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) =
    MkCartesianCoord'' (r * cos phi, r * sin phi)

-- Sprawdzić działanie funkcji polarToCartesian'' dla poprawnych i niepoprawnych
-- danych wejściowych; rozważyć zalety i wady powyższych trzech implementacji

personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (name, surname, address) =
   "name: " ++ name ++ ", surname: " ++ surname ++ ", addr: " ++ address

newtype PersonInfo'' = MkPersonInfo (Name', Surname', Address')
personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (MkPersonInfo (name, surname, address)) =
   "name: " ++ name ++ ", surname: " ++ surname ++ ", addr: " ++ address

-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a
xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}
-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y

p23 = MkCart2DVec'' {x = 2, y = 3}

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

{-
  1. Zdefiniować funkcje dostępowe np. xCoord3D itd. dla następującego typu (reprezentacja wektorów w 3D, współrzędne kartezjańskie)
-}

{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)

 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = Cart3DVec a a a
x3DCoord :: Cart3DVec a -> a
x3DCoord (Cart3DVec x _ _) = x

y3DCoord :: Cart3DVec a -> a
y3DCoord (Cart3DVec _ y _) = y

z3DCoord :: Cart3DVec a -> a
z3DCoord (Cart3DVec _ _ z) = z

{-
  Wykorzystując record syntax napisać nową wersję Cart3DVec, a następnie sprawdzić istnienie odpowiednich (wygenerowanych przez kompilator) funkcji dostępowych
-}
data Cart3DVec' a = Cart3DVec' {x3::a, y3::a, z3::a}

{-
  (opcjonalne) Zdefiniować odpowiednie wersje typów (bez i z record syntax) dla wektorów 2D w układzie biegunowym
-}
data Polar2DVec a = Polar2DVec a a
rCoord :: Polar2DVec a -> a
rCoord (Polar2DVec r _) = r

alfaCoord :: Polar2DVec a -> a
alfaCoord (Polar2DVec _ alfa) = alfa

data Polar2DVec' a = Polar2DVec' {r::a, alfa::a}

polarToCartesian2 :: Floating a => Polar2DVec' a -> Cart2DVec'' a
polarToCartesian2 (Polar2DVec' r alfa) = (MkCart2DVec'' (r * cos alfa)  (r * sin alfa))

{-
  (opcjonalne) Napisać odpowiedniki powyższych typów i funkcji dla układów: cylindrycznego i sferycznego
-}

--

{-
  Zdefiniować funkcję obliczającą pole powierzchni (figury płaskiej)
-}
data Shape = Circle Float |
             Rectangle Float Float
area :: Shape -> Float

--

{-
  Zdefiniować funkcję zwracającą element przechowywany w korzeniu drzewa binarnego zdefiniowanego w następujący sposób.
  Uwaga: dla pustego drzewa funkcja powinna zgłaszać błąd (funkcja error)
-}
rootValue :: Tree a -> a
data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

--

{-
  Zdefiniować typ wyliczeniowy TrafficLights dla sygnalizacji świetlnej oraz funkcję
  podającą, co powinien robić kierowca, widząc dane światło
-}
actionFor :: TrafficLights -> String

--

{-
  (opcjonalne) Zdefiniować typ wyliczeniowy dla ‘akcji’ z poprzedniego zadania (działanie kierowcy w danej sytuacji)
-}
