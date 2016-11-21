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
