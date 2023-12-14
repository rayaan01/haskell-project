{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parse (parseCSV) where

import Data.CSV
import Text.ParserCombinators.Parsec
import Data.Either
import System.IO
import Types
-- import Data.Text (Text, pack)
import Data.Typeable


parseRecord :: [String] -> RecordCsv
parseRecord record = Record { r_id = read $ record !! 0 , r_country = record !! 1, year = record !! 2, gdp = record !! 4}

filterGDP :: [String] -> Bool
filterGDP (_:_:_:x:xs)= x == "GDP in current prices (millions of US dollars)"

filterYear :: [String] -> Bool
filterYear (_:_:x:xs)= x == "2010" || x == "2015" || x == "2021"

parseGDP :: [[String]] ->  [RecordCsv]
parseGDP csvData  = do
  let newcsvData = drop 842 csvData
  let filteredRecord = filter filterGDP newcsvData
  let newFilteredRecord = filter filterYear filteredRecord
  map parseRecord newFilteredRecord

parseCSV :: CSVFiles -> IO [RecordCsv]
parseCSV fileCSV = do
    handle <- openFile (gdpf fileCSV ++ ".csv") ReadMode
    hSetEncoding handle char8
    contents <- hGetContents handle
    let result = parse csvFile "" contents
    -- Required, so we don't close the handle before dooing
    let csvData = fromRight [["invalid"]] result
    let records = parseGDP csvData
    putStrLn "Forcing Evlauation of Result. "
    print $ take 1 csvData
    -- Dropping all non-Countries from the GDP List
    -- putStrLn $ show records
    hClose handle
    -- let result = map pack result
    return records
    -- readListPrecDefaultlt <- parseFromFile csvFile fileCSV
    -- pure $ records


