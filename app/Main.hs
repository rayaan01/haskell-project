module Main (main) where

import Fetch (downloadURLS, getURLConstructor) 
import Parse
import Types
import Database

-- | The Main function
main :: IO ()
main = do
  -- Fetching
  let dataUN = getURLConstructor "UN"
  let files = CSVFiles { gdpf = "gdp", popf =  "pop"}

  downloadURLS dataUN

  -- Parsing

  records <- parseCSV files
  -- Database
  createTables

  mapM_ addPopulation popData  
  addGDP records -- "India"
  putStrLn "Data Added Succesfully!"

