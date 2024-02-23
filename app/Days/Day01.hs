module Days.Day01 where

import Data.Char (isDigit)
import Data.List
import Data.Maybe
import Debug.Trace (traceIO)

-- Safely parse a string into an integer, returning Nothing if parse fails
safeReadInt :: String -> Maybe Int
safeReadInt s = case wordToNum s of
  Just num -> Just num
  Nothing -> case filter isDigit s of
    "" -> Nothing
    str -> Just (read str)

wordToNum :: String -> Maybe Int
wordToNum s = case s of
  "one" -> Just 1
  "two" -> Just 2
  "three" -> Just 3
  "four" -> Just 4
  "five" -> Just 5
  "six" -> Just 6
  "seven" -> Just 7
  "eight" -> Just 8
  "nine" -> Just 9
  _ -> Nothing

parseInput :: String -> [Int]
parseInput = mapMaybe safeReadInt . lines

combineDigits :: Int -> Int
combineDigits n =
  let strN = show n
   in case length strN of
        1 -> n * 11
        _ -> read [head strN, last strN]

solvePart1AndPrint :: [Int] -> IO ()
solvePart1AndPrint digits = do
  let result = solvePart1 digits
  putStrLn $ "Result: " ++ show result

--  Sum of the array of integers
solvePart1 :: [Int] -> Int
solvePart1 = sum . map combineDigits

extractDigits :: String -> [Int]
extractDigits s =
  let wordReplacements = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]
      replaceWordWithDigit (word, digit) line = replace word digit line
      replacedLines = foldl (\acc replacement -> map (replaceWordWithDigit replacement) acc) [s] wordReplacements
      _ = traceIO ("Replaced lines: " ++ show replacedLines)
      _ = traceIO ("Concatenated lines: " ++ show replacedLines)
      remainingDigits = mapMaybe safeReadInt $ filter isDigit <$> replacedLines
   in remainingDigits

-- replaceWord :: String -> String -> String -> String
-- replaceWord old new s = intercalate new $ splitOn old s

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace old new line = go line
 where
  go [] = []
  go s@(x : xs)
    | isPrefixOf old s = new ++ replace old new (drop (length old) s)
    | otherwise = x : replace old new xs

-- Extract and print digits for each linesOfFile
printExtractedDigits :: [String] -> IO ()
printExtractedDigits linesOfFile = do
  let extractedDigits = map extractDigits linesOfFile
  mapM_ print extractedDigits

--  Sum of the array of integers
-- solvePart2 :: [String] -> Int
-- solvePart2 linesOfFile = do
