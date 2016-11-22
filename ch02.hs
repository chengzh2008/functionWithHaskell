{-|
  Exercise D

  Beaver: n times
  Susan: 1 time

  head . map f = f . head

  filter :: (a -> Bool) -> [a] -> [b]
  why would beaver not use the expression : head . filter p

-}

first :: (a -> Bool) -> [a] -> a
first p xs
  | null xs = error "Empty list"
  | p x = x
  | otherwise = (first p . tail) xs
  where x = head xs

-- head . filter p . map f = head . filter (p . f)
first2 :: (a -> Bool) -> (a -> a) -> [a] -> a
first2 p f xs
  | null xs = error "Empty list"
  | p x = x
  | otherwise = first2 p f (tail xs)
  where x = f (head xs)


{-|
  exercise E
  use Maybe to handle failure

-}
firstMaybe :: (a -> Bool) -> [a] -> Maybe a
firstMaybe p xs
  | null xs = Nothing
  | p x = Just x
  | otherwise = firstMaybe p . tail $ xs
  where x = head xs


{-|
  exercise F

  n - 1 miltiplication for the exp funciton
-}
exp1 :: Integer -> Integer -> Integer
exp1 x n
  | n == 0 = 1
  | n == 1 = x
  | even n = exp1 (x * 2) m
  | odd n = x * exp1  (x * 2) m
  where m = n `div` 2
-- floor log n times multiplication

{-|
  exercise G

  showDate (10, 12, 2013) = "10th December, 2013"
-}

type Date = (Int, Int, Int)
showDate :: Date -> String
showDate (d, m, y) = showDay d ++ " " ++ showMonth m ++ ", " ++ show y
showDay :: Int -> String
showDay n
  | n == 1 || n == 21 || n == 31 = show n ++ "st"
  | n == 2 || n == 22 = show n ++ "nd"
  | n == 3 || n == 23 = show n ++ "rd"
  | otherwise = show n ++ "th"
showMonth :: Int -> String
showMonth m
  | m < 1 || m > 12 = error "Invalid month"
  | otherwise =  months !! (m-1)
    where months  = ["Jan", "Feb", "Mar", "Apri", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]


{-|
  exercise H
  ten-digit card identification numbers (CINs)

-}
