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
