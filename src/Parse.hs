module Parse (parseCSV) where

import Data.CSV
import Text.ParserCombinators.Parsec
import Data.Either
import System.IO
import Types


parseRecord :: [String] -> RecordCsv
parseRecord record = Record { r_id = read (head record) , r_country = record !! 1, year = record !! 2, gdp = (record !! 4 )}

filterGDP :: [String] -> Bool
filterGDP (_:_:_:x:xs)= x == "GDP in current prices (millions of US dollars)"

filterYear :: [String] -> Bool
filterYear (_:_:x:xs)= x == "2010" || x == "2015" || x == "2021"

parseGDP :: [[String]] ->  IO ()
parseGDP csvData  = do
  let newcsvData = drop 842 csvData
  let filteredRecord = filter filterGDP newcsvData
  let newFilteredRecord = filter filterYear filteredRecord
  let csvRecord =  map parseRecord newFilteredRecord
  print $ take 10 csvRecord

parseCSV :: CSVFiles -> IO ()
parseCSV fileCSV = do
    handle <- openFile ((gdpf fileCSV) ++ ".csv") ReadMode
    hSetEncoding handle char8
    contents <- hGetContents handle
    let result =  parse csvFile "" contents
    let csvData = fromRight [["invalid"]] result
    -- print $ take 10 csvData
    -- Dropping all non-Countries from the GDP List
    parseGDP csvData
    hClose handle
    -- result <- parseFromFile csvFile fileCSV


