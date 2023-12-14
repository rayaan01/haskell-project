module Main (main) where

import Fetch (downloadURLS, getURLConstructor) 
import Parse
import Types
import Database
import Parse (getGDP, getPOP)

-- | The Main function
main :: IO ()
main = do
  -- Fetching
  let dataUN = getURLConstructor "UN"
  let files = CSVFiles { gdpf = "gdp", popf =  "pop"}

  downloadURLS dataUN

  -- Parsing
  gdpData <- getGDP
  popData <- getPOP

  -- Database
  createTables
  -- callMain popData 
  callMain gdpData 
  putStrLn "Data Added Succesfully!"
  fetchGDP "United Kingdom" "2015"
  fetchPopulation "USA" "2015"
