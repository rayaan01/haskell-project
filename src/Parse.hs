{-# LANGUAGE BangPatterns #-}
module Parse (getGDP, getPOP) where

import Data.CSV
import Text.ParserCombinators.Parsec
import System.IO
import Types
import Data.Either

parseGDPRecord :: [String] -> RecordGDP
parseGDPRecord record = RecordGDP {
  g_id = read (head record),
  g_country = record !! 1,
  g_year = record !! 2,
  gdp = record !! 4
}

parsePOPRecord :: [String] -> RecordPOP
parsePOPRecord record = RecordPOP {
  p_id = read (head record),
  p_country = record !! 1,
  p_year = record !! 2,
  pop = record !! 4
}

filterGDP :: [String] -> Bool
filterGDP x = x !! 3 == "GDP in current prices (millions of US dollars)"

filterPOP :: [String] -> Bool
filterPOP x = x !! 3 == "Population mid-year estimates (millions)"

filterYear :: [String] -> Bool
filterYear x = x !! 2 == "2010" || x !! 2 == "2015" || x !! 2 == "2021"

parseGDP :: [[String]] ->  [RecordGDP]
parseGDP csvData  = do
  let newcsvData = drop 842 csvData
  let filteredRecord = filter filterGDP newcsvData
  let newFilteredRecord = filter filterYear filteredRecord
  map parseGDPRecord newFilteredRecord

parsePOP :: [[String]] -> [RecordPOP]
parsePOP csvData = do
  let newcsvData = drop 842 csvData
  let filteredRecord = filter filterPOP newcsvData
  let newFilteredRecord = filter filterYear filteredRecord
  map parsePOPRecord newFilteredRecord

getGDP :: IO [RecordGDP]
getGDP = do
    let filePath = "gdp.csv"
    handle <- openFile filePath ReadMode
    hSetEncoding handle char8
    contents <- hGetContents handle
    let parseRes = parse csvFile filePath contents
    let csvData = fromRight [["invalid"]] parseRes  
    let !records = parseGDP csvData
    hClose handle
    return records

getPOP :: IO [RecordPOP]
getPOP = do
    let filePath = "pop.csv"
    handle <- openFile filePath ReadMode
    hSetEncoding handle char8
    contents <- hGetContents handle
    let parseRes = parse csvFile filePath contents
    let csvData = fromRight [["invalid"]] parseRes  
    let !records = parsePOP csvData
    hClose handle
    return records