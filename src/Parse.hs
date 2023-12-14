{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Parse (parseCSV) where

import Data.CSV
import Text.ParserCombinators.Parsec
import Data.Either
import System.IO
-- import Data.Typeable

data RecordGDP = GDP {
  id :: Int,
  country :: String,
  y_2005:: Int,
  y_2010 :: Int,
  y_2015 :: Int,
  y_2020 :: Int,
  capital :: String
}

data RecordCsv = Record {
  r_id :: Int,
  r_country :: String,
  year:: String,
  gdp :: String
} deriving (Show)

parseRecord :: [String] -> RecordCsv
parseRecord record = Record {
  r_id = read (head record),
  r_country = record !! 1,
  year = record !! 2,
  gdp = record !! 4
}

filterGDP :: [String] -> Bool
filterGDP x = x !! 3 == "GDP in current prices (millions of US dollars)"

filterYear :: [String] -> Bool
filterYear x = x !! 2 == "2010" || x !! 2 == "2015" || x !! 2 == "2021"

parseGDP :: [[String]] ->  IO ()
parseGDP csvData  = do
  -- print $ typeOf $ (take 1 csvData) !! 0
  -- print $ parseRecord $ (take 1 csvData) !! 0
  let filteredRecord = filter filterGDP csvData
  let newFilteredRecord = filter filterYear filteredRecord
  let csvRecord =  map parseRecord newFilteredRecord
  print $ take 10 csvRecord

parseCSV :: String -> IO ()
parseCSV fileCSV = do
    handle <- openFile fileCSV ReadMode
    hSetEncoding handle char8
    contents <- hGetContents handle
    let result =  parse csvFile "" contents
    let csvData = fromRight [["invalid"]] result
    -- print $ take 10 csvData
    parseGDP $ drop 842 csvData
    hClose handle
    -- result <- parseFromFile csvFile fileCSV