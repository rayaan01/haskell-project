module Main where

import Database

main :: IO ()
main = do
    createTables
    mapM_ addPopulation popData
    mapM_ addGDP gdpData
    putStrLn "Data Added Succesfully!"