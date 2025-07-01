module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "trying to get maximum of an empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

minimum' :: (Ord a) => [a] -> a
minimum' [] = error "trying to get minimum of an empty list"
minimum' [x] = x
minimum' (x:xs)
  | x < minTail = x
  | otherwise = minTail
  where minTail = minimum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a] -- not all Num are Ord (eg complex) and vice versa
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n-1) x

-- ^ notice that guards turn out to be more convenient than patterns in some settings

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n x
  | n <= 0     = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- ^ mixing guards and patterns is allowed, as long as all cases are covered.

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' (xs) (ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    =  True
  | otherwise =  elem' a (xs)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let smallerSorted = qsort [a | a<-xs, a <= x]
      biggerSorted  = qsort [a | a<-xs, a >  x]
  in  smallerSorted ++ [x] ++ biggerSorted

-- ^ quicksort implemented in 6 lines :-P

