module Main (main) where

import Fetch (downloadURLS, getURLConstructor) 
import Parse
import Types
import Database

-- | The Main function
main :: IO ()
main = do
  -- Fetching
  let dataUn = getURLConstructor "UN"

  downloadURLS dataUn

  -- Parsing

  parseCSV "gdp.csv"

  -- Database Operations

  createTables
  mapM_ addPopulation popData
  mapM_ addGDP gdpData
  putStrLn "Data Added Succesfully!"