{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Parse (parseCSV) where

import Data.CSV
import Text.ParserCombinators.Parsec
import Data.Either
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
  gdp :: Int
} deriving (Show)

parseRecord :: [String] -> RecordCsv
parseRecord record = Record { r_id = read (head record) , r_country = record !! 1, year = record !! 2, gdp = read $ filter (/=',') (record !! 4 )}


parseGDP :: [[String]] ->  IO ()
parseGDP csvData  = do
  -- print $ typeOf $ (take 1 csvData) !! 0
  -- print $ parseRecord $ (take 1 csvData) !! 0
  let csvRecord =  map parseRecord csvData
  print $ head csvRecord

parseCSV :: String -> IO ()
parseCSV fileCSV = do
    result <- parseFromFile csvFile fileCSV
    let csvData = fromRight [["invalid"]] result
    parseGDP $ drop 2 csvData

