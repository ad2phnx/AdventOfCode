module Main where

import Days.Day01

main :: IO ()
main = do
  contents <- readFile "input/day01.txt"
  let input = parseInput contents
  let linesOfFile = lines contents
  solvePart1AndPrint input
  printExtractedDigits linesOfFile

-- print $ solvePart2 linesOfFile
