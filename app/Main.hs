module Main (main) where

import Parse

main :: IO ()
main = do
    parseCSV "gdp.csv"
