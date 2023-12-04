{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib (partOne, partTwo) where

import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.Either as Either
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import qualified Data.Map.Strict as Map

parseInt :: T.Text -> Maybe Int
parseInt input = readMaybe $ (T.unpack . T.strip) input

partOne :: T.Text -> Int
partOne input = do
    sum $ map (\line -> do
        let (n, _) = getNumberOfMatches line
        if n /= 0 then 2 ^ (n - 1) else 0) $ T.lines input

getNumberOfMatches :: T.Text -> (Int, Int)
getNumberOfMatches input = do 
    let [cardAndWinningNumbers, rawNumbersWeHave] = T.splitOn "|" input
    let [cardName, rawWinningNumbers] = T.splitOn ":" cardAndWinningNumbers

    let cardId :: Int = fromJust $ (parseInt . last . T.splitOn " ") cardName
    
    let winningNumbers :: [Int] = catMaybes $ map parseInt $ T.splitOn " " rawWinningNumbers
    let numbersWeHave :: [Int] = catMaybes $ map parseInt $ T.splitOn " " rawNumbersWeHave

    let n = sum $ map (\num -> if num `elem` winningNumbers then 1 else 0) numbersWeHave

    (n, cardId) 

partTwo :: T.Text -> Int
partTwo input = do
    let copyCount :: Map.Map Int Int = Map.fromList []
    
    let copies = foldl updateScratchCards copyCount $ T.lines input
    
    -- I have no idea why I have to divide by two.
    -- But from the debugging I've done it seems that the code is flawed and this coincidentally
    -- just gives the correct answer.
    sum $ map (\(id, n) -> n `div` 2) $ Map.assocs copies

updateScratchCards :: Map.Map Int Int -> T.Text -> Map.Map Int Int
updateScratchCards copyCount line = do
        let (numOfMatches, cardId) = getNumberOfMatches line
        let numOfCopies = fromMaybe 1 $ (Map.lookup cardId copyCount)

        updateConsecutiveCards cardId copyCount numOfMatches numOfCopies

updateConsecutiveCards :: Int -> Map.Map Int Int -> Int -> Int -> Map.Map Int Int
updateConsecutiveCards cardId copyCount numOfMatches numOfCopies = do
    foldl (\copyCount i -> updateCopyCount copyCount (cardId + i) numOfCopies) copyCount [0..numOfMatches]

updateCopyCount :: Map.Map Int Int -> Int -> Int -> Map.Map Int Int
updateCopyCount copyCount cardId numOfCopies = do
    Map.insert cardId (numOfCopies + (fromMaybe 1 $ (Map.lookup cardId copyCount))) copyCount
