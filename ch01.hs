{-|
   functional programming: a method of program construction that emphasises functions and their application rather than commands and their execution.
   functional programming uses simple methematical notation that allows problems to be described clearly and concisely
   function programming has a simple methmatical basis that supports equational reasoning about the properties of programs
-}
import Data.Char
import Data.List

{-|
commonWords :: Int -> Text -> String
commonWords n = concat . map showRun . take n . sortRuns . countRuns . sortWords . words . map toLower
-}

{-|
   number to words
   convert a non-negative number less than one million to a string representing the number in words.
 -}

units, teens, tens :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- for 0 <= n < 10
convert1 :: Int -> String
convert1 n = units !! n
-- for 0 <= n < 100

convert2 :: Int -> String
convert2 x
  | m == 0 = units !! n
  | m == 1 = teens !! n
  | 2 <= m && n == 0 = tens !! (m - 2)
  | 2 <= m && n /= 0 = tens !! (m - 2) ++ "-" ++ units !! n
  where (m, n) = (x `div` 10, x `mod` 10)

-- for 0 <= n < 100
convert3 :: Int -> String
convert3 n
  | h == 0 = convert2 t
  | t == 0 = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundred and " ++ convert2 t
  where (h, t) = (n `div` 100, n `mod` 100)
        --
-- for 0 <= n < 1000,000
convert6 :: Int -> String
convert6 n
  | h == 0 = convert3 t
  | t == 0 = convert3 h ++ " thousand"
  | otherwise = convert3 h ++ " thousand" ++ link t ++ convert3 t
  where (h, t) = (n `div` 1000, n `mod` 1000)

link :: Int -> String
link t
  | t < 100 = " and "
  | otherwise = " "

-- function name for export
-- convert number between 0 and 1,000,000 to words representing the number
convert :: Int -> String
convert = convert6

{-|
  Exercise D
  words . map toLower = map (map toLower) . words
-}

{-|
  Exercise F
  print a song
-}

song :: Int -> String
song n
  | n == 0 = ""
  | otherwise = song (n-1) ++ "\n" ++ verse n
  where
    verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

line1, line2, line3, line4 :: Int -> String
line1 n
  | n == 1 = "One man went to mow\n"
  | otherwise = numbers !! (n-2) ++ " men went to mow\n"

line2 n = "Went to mow a meadow\n"

line3 n
  | n == 1 = "One man and his dog\n"
  | otherwise = numbers !! (n-2) ++ " men, " ++ count (n-2) ++ "one man and his dog\n"

line4 n = "Went to mow a meadow\n\n"

count :: Int -> String
count n
  | n == 0 = ""
  | otherwise = numbs !! (n-1) ++ " men, " ++ count (n-1)

numbers = ["Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
numbs = init . map (map toLower) $ numbers
