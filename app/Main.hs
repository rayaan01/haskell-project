module Main (main) where
import Fetch (downloadURLS, getURLConstructor) 
import Database
import Parse (getGDP, getPOP)

-- | The Main function
main :: IO ()
main = do
  -- Fetching
  -- | Fetching the data from UN website
  let dataUN = getURLConstructor "UN"
  downloadURLS dataUN

  -- Parsing
  -- | Parsing GDP and Population data
  gdpData <- getGDP
  popData <- getPOP

  -- Database
  -- | Creating the ddatabse and saving the data.
  createTables
  savePOPData popData
  saveGDPData gdpData 
  putStrLn "Data Added Succesfully!"

  -- | Fetching and displaying GDP and Population data for a specific country and year.
  fetchGDP "United Kingdom" "2015"
  fetchPopulation "Austria" "2015"