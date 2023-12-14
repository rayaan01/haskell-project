module Main (main) where

import Fetch (downloadURLS, getURLConstructor) 
import Parse
import Types
import Database

-- | The Main function
main :: IO ()
main = do
  -- Fetching - Can pass "UN" or "World Bank"
  let dataUN = getURLConstructor "UN"
  downloadURLS dataUN

  -- Parsing
  parseCSV "gdp.csv"

  -- Database Operations
  createTables
  mapM_ addPopulation popData
  mapM_ addGDP gdpData
  putStrLn "Data Added Succesfully!"