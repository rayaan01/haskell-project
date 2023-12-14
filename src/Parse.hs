{-# LANGUAGE BangPatterns #-}
module Parse (parseCSV) where

import Data.CSV
import Text.ParserCombinators.Parsec
import System.IO
import Types
import Data.Either

parseRecord :: [String] -> RecordGDP

parseRecord record = RecordGDP {
  r_id = read (head record),
  r_country = record !! 1,
  year = record !! 2,
  gdp = record !! 4
}

filterGDP :: [String] -> Bool
filterGDP x = x !! 3 == "GDP in current prices (millions of US dollars)"

filterYear :: [String] -> Bool
filterYear x = x !! 2 == "2010" || x !! 2 == "2015" || x !! 2 == "2021"

parseGDP :: [[String]] ->  [RecordGDP]
parseGDP csvData  = do
  let newcsvData = drop 842 csvData
  let filteredRecord = filter filterGDP newcsvData
  let newFilteredRecord = filter filterYear filteredRecord
  map parseRecord newFilteredRecord

parseCSV :: CSVFiles -> IO [RecordGDP]
parseCSV fileCSV = do
    handle <- openFile (gdpf fileCSV ++ ".csv") ReadMode
    hSetEncoding handle char8
    contents <- hGetContents handle
    -- let result = parse csvFile "" contents
    let parseRes = parse csvFile "gdb.csv" contents
    let csvData = fromRight [["invalid"]] parseRes  
    let !records = parseGDP csvData

    hClose handle
    -- let result = map pack result
    return records
