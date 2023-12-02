{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib (partOne, partTwo) where

import qualified Data.Text as T

partOne :: T.Text -> Int
partOne input = do
    let validGameIds :: [Int] = map getIdOfValidGame $ T.lines input
    foldl (+) 0 validGameIds

partTwo :: T.Text -> Int
partTwo input = do
    let colorProducts :: [Int] = map (colorProduct . getPossibleColorCombination) $ T.lines input
    foldl (+) 0 colorProducts

colorProduct :: (Int, Int, Int) -> Int
colorProduct (redCubes, blueCubes, greenCubes) = redCubes * blueCubes * greenCubes

-- Returns the smallest possible combination of colored cubes that could represent that game.
getPossibleColorCombination :: T.Text -> (Int, Int, Int)
getPossibleColorCombination game = do
    let [_, gameRounds] = T.splitOn ":" game
    let parsedRounds = map (T.splitOn ", ") $ T.splitOn ";" gameRounds
    let (reds, blues, greens) = unzip3 $ map getColorCount parsedRounds
    
    (maximum reds, maximum blues, maximum greens)


-- Given a game, returns its game id if the game is valid otherwise 0. The criteria for validity is determined
-- by the isValidGame function.
getIdOfValidGame :: T.Text -> Int
getIdOfValidGame game = do
    let [gameId, gameRounds] = T.splitOn ":" game
    let parsedGameId :: Int = (read . T.unpack) $ (last $ T.words gameId)
    let parsedRounds = map (T.splitOn ", ") $ T.splitOn ";" gameRounds
    
    let listOfValidRounds = map (isValidRound . getColorCount) parsedRounds
    
    if all (==True) listOfValidRounds
        then parsedGameId
        else 0

-- Returns the number of Red, Blue, and Green cubes shown in a single game round.
-- If the round doesn't contain a certain cube, it is assumed to be 0.
getColorCount :: [T.Text] -> (Int, Int, Int)
getColorCount gameRound = foldl updateColorCount (0, 0, 0) gameRound where

    updateColorCount :: (Int, Int, Int) -> T.Text -> (Int, Int, Int)
    updateColorCount (red, blue, green) colors = do 
        let [amount, color] = T.splitOn " " $ T.strip colors
        let colorCount = read $ T.unpack amount
        case color of
            "blue" -> (red, blue + colorCount, green)
            "red" -> (red + colorCount, blue, green)
            "green" -> (red, blue, green + colorCount)

isValidRound :: (Int, Int, Int) -> Bool
isValidRound (redCubes, blueCubes, greenCubes) = redCubes <= 12 && blueCubes <= 14 && greenCubes <= 13
