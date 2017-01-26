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
