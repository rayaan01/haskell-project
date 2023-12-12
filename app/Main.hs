module Main (main) where

import Data.CSV
import Text.ParserCombinators.Parsec

removeComma = filter (/= ',')

main :: IO ()
main = do
    result <- parseFromFile csvFile "gdp.csv"
    putStrLn $ show result
