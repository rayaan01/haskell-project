module Main (main) where

import Fetch (downloadURLS, getURLConstructor) 
import Database
import Parse (getGDP, getPOP)

-- | The Main function
main :: IO ()
main = do
  -- Fetching
  let dataUN = getURLConstructor "UN"
  downloadURLS dataUN

  -- Parsing
  gdpData <- getGDP
  popData <- getPOP

  -- Database
  createTables
  savePOPData popData
  saveGDPData gdpData 
  putStrLn "Data Added Succesfully!"
  fetchGDP "United Kingdom" "2015"
  fetchPopulation "Austria" "2015"