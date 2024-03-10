module Days.Day01 where

import Data.Char (chr, digitToInt, intToDigit, isDigit, ord)
import Data.List (elemIndex, find, intercalate, isInfixOf, isPrefixOf, maximumBy, minimumBy, sort)
import Data.Maybe
import Debug.Trace (traceIO)

import Data.Function (on)

-- Safely parse a string into an integer, returning Nothing if parse fails
safeReadInt :: String -> Maybe Int
safeReadInt s = case wordToNum s of
  Just num -> Just num
  Nothing -> case filter isDigit s of
    "" -> Nothing
    str -> Just (read str)

keywordList :: [String]
keywordList = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

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

keywordToNumber :: String -> Maybe Int
keywordToNumber keyword =
  case keyword of
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

numberToKeyword :: Char -> Maybe String
numberToKeyword digit
  | isDigit digit = case digit of
      '0' -> Just "zero"
      '1' -> Just "one"
      '2' -> Just "two"
      '3' -> Just "three"
      '4' -> Just "four"
      '5' -> Just "five"
      '6' -> Just "six"
      '7' -> Just "seven"
      '8' -> Just "eight"
      '9' -> Just "nine"
      _ -> Nothing
  | otherwise = Just [digit]

replaceDigitsWithWords :: String -> String
replaceDigitsWithWords = concatMap (\c -> maybe [c] id (numberToKeyword c))

-- Function to check if keyword exists in string
containsKeyword :: String -> String -> Bool
containsKeyword keyword text = isInfixOf keyword text

-- Function to get the index of keyword in a string
getKeywordIndex :: String -> String -> Maybe Int
getKeywordIndex keyword text
  | containsKeyword keyword text = Just (length $ takeWhile (/= keyword) $ iterate (drop 1) text)
  | otherwise = Nothing

-- Function to get all indices of a keyword in a string
getAllKeywordIndices :: String -> String -> [Int]
getAllKeywordIndices keyword text = go 0 text
 where
  go n xs
    | length xs < length keyword = []
    | keyword `isPrefixOf` xs = n : go (n + 1) (drop 1 xs)
    | otherwise = go (n + 1) (tail xs)

getMinMaxKeywordIndices :: String -> String -> Maybe (Int, Int)
getMinMaxKeywordIndices keyword text
  | null indices = Nothing
  | otherwise = Just (minimum indices, maximum indices)
 where
  indices = getAllKeywordIndices keyword text

iterateKeywords :: [String] -> String -> [(String, Maybe (Int, Int))]
iterateKeywords keywords text = [(keyword, getMinMaxKeywordIndices keyword text) | keyword <- keywords]

getKeywordWithSmallestMinIndex :: [(String, Maybe (Int, Int))] -> Maybe (String, (Int, Int))
getKeywordWithSmallestMinIndex results
  | null validResults = Nothing
  | otherwise = Just $ minimumBy (compare `on` (\(_, (minIndex, _)) -> minIndex)) validResults
 where
  validResults = [(kw, indices) | (kw, Just indices) <- results]

getKeywordWithLargestMaxIndex :: [(String, Maybe (Int, Int))] -> Maybe (String, (Int, Int))
getKeywordWithLargestMaxIndex results
  | null validResults = Nothing
  | otherwise = Just $ maximumBy (compare `on` (\(_, (_, maxIndex)) -> maxIndex)) validResults
 where
  validResults = [(kw, indices) | (kw, Just indices) <- results]

getKeywordMinMaxPerLine :: [String] -> String -> [(Maybe (String, (Int, Int)), Maybe (String, (Int, Int)))]
getKeywordMinMaxPerLine keywords text = map getMinMaxPerLine (lines text)
 where
  getMinMaxPerLine line = (getKeywordWithSmallestMinIndex (iterateKeywords keywords line), getKeywordWithLargestMaxIndex (iterateKeywords keywords line))

handleKeywordMinMaxPerLine :: [(Maybe (String, (Int, Int)), Maybe (String, (Int, Int)))] -> Maybe Int
handleKeywordMinMaxPerLine results = sumTwoDigitRepresentations results

getTwoDigitRepresentation :: Maybe (String, (Int, Int)) -> Maybe (String, (Int, Int)) -> Maybe String
getTwoDigitRepresentation minResult maxResult =
  case (minResult, maxResult) of
    (Just (minKeyword, _), Just (maxKeyword, _)) -> do
      minNumber <- keywordToNumber minKeyword
      maxNumber <- keywordToNumber maxKeyword
      Just $ show (minNumber * 10 + maxNumber)
    _ -> Nothing

sumTwoDigitRepresentations :: [(Maybe (String, (Int, Int)), Maybe (String, (Int, Int)))] -> Maybe Int
sumTwoDigitRepresentations results = do
  let twoDigitReps = map (\(minResult, maxResult) -> getTwoDigitRepresentation minResult maxResult) results
  numbers <- sequence $ map readMaybe twoDigitReps
  Just $ sum numbers
 where
  readMaybe :: Maybe String -> Maybe Int
  readMaybe = fmap read

--  Sum of the array of integers
solvePart1 :: [Int] -> Int
solvePart1 = sum . map combineDigits

solvePart1AndPrint :: String -> IO ()
solvePart1AndPrint contents = do
  let digits = parseInput contents
  let result = solvePart1 digits
  putStrLn $ "Result Part1: " ++ show result

solvePart2 :: String -> String
solvePart2 file = replaceDigitsWithWords file

solvePart2AndPrint :: String -> IO ()
solvePart2AndPrint contents = do
  let result = solvePart2 contents
  let lineResults = getKeywordMinMaxPerLine keywordList result
  let totalSum = handleKeywordMinMaxPerLine lineResults
  putStrLn $ "Result Part2: " ++ show totalSum
