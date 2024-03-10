module Main where

import Days.Day01

main :: IO ()
main = do
  contentsD1 <- readFile "input/day1.txt"
  solvePart1AndPrint contentsD1
  solvePart2AndPrint contentsD1
  contentsD2 <- readFile "input/day2.txt"
  solvePart1AndPrint contentsD2

-- solvePart2AndPrint contentsD2
