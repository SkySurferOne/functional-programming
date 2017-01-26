-- back to past
say = putStr "Write your name: " >> getLine >>= \n -> putStrLn $ "Hello majestic " ++ n ++ "!"

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

-- data R a = R a
-- instance Monad R where
--   return a = R a
--   (R a) >>= f = f a

data Box a = MkBox a
incIntBox :: Int -> Box Int
incIntBox x = MkBox (x + 1)

f :: Int -> Box Int
g :: Int -> Box Int
f x = MkBox (x + 1)
g x = MkBox (x * 2)

--
(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x
infixl 0 >$>

(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.>

extractMaybe :: Maybe a -> a
extractMaybe Nothing  = error "Nothing inside!"
extractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe = Just

-- (>^$>) = extract (^) and apply ($)
-- (>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- ma >^$> f = (extractMaybe ma) >$> f
-- infixl 1 >^$>

(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >^$> _ = Nothing
(Just x) >^$> f = f x
infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

-- Kleisli composition
(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >.>> g = \x -> g (extractMaybe (f x))
