module Main (main) where

import Lib
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    testInput <- TIO.readFile "test_input_part1.txt"
    puzzleInput <- TIO.readFile "puzzle_input.txt"

    putStrLn "Part 1:"
    putStrLn $ "Test Input :: " ++ (show $ partOne testInput)
    putStrLn $ "Puzzle Input :: " ++ (show $ partOne puzzleInput)

    putStrLn "Part 2:"
    putStrLn $ "Test Input :: " ++ (show $ partTwo testInput)
    putStrLn $ "Puzzle Input :: " ++ (show $ partTwo puzzleInput)
