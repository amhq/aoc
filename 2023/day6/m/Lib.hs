{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib (partOne, partTwo) where

import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromJust)

parseInt :: T.Text -> Maybe Int
parseInt input = readMaybe $ (T.unpack . T.strip) input

parseWords :: T.Text -> [T.Text]
parseWords text = do
    let wordsWithEmptyText = map T.strip (T.words $ T.strip text)
    filter (\s -> s /= "") wordsWithEmptyText

partOne :: T.Text -> Int
partOne input = do
    let [[_, rawTimes], [_, rawDistances]] = map (T.splitOn ":") $ T.lines input
    let raceTimes :: [Int] = catMaybes $ (map parseInt (T.words $ T.strip rawTimes))
    let raceDistances :: [Int] = catMaybes $ (map parseInt (T.words $ T.strip rawDistances))
    let raceDetails = zip raceTimes raceDistances

    product $ map (\(raceTime, raceDistance) -> do
        let possibleDistances = map (\i -> (raceTime - i) * i) [0..raceTime]
        let numberOfRecordBreakers = length $ filter (\distance -> distance > raceDistance) possibleDistances

        numberOfRecordBreakers
        ) raceDetails

partTwo :: T.Text -> Int
partTwo input = do
    let [[_, rawTime], [_, rawDistance]] = map (T.splitOn ":") $ T.lines input
    let raceTime :: Int = fromJust $ readMaybe $ T.unpack (T.concat $ parseWords rawTime)
    let raceDistance :: Int = fromJust $ readMaybe $ T.unpack (T.concat $ parseWords rawDistance)

    let possibleDistances = map (\i -> (raceTime - i) * i) [0..raceTime]
    let numberOfRecordBreakers = length $ filter (\distance -> distance > raceDistance) possibleDistances

    numberOfRecordBreakers
