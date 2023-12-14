module Main (main) where
import Fetch (downloadURLS, getURLConstructor)
import Database
import Parse (getGDP, getPOP)
import System.Exit (die)

nameToBeDetermined :: IO ()
nameToBeDetermined = do
  putStrLn "\n\n1 - Fetch population of a country"
  putStrLn "2 - Fetch GDP of a country"
  putStrLn "3 - Fetch both Population and GDP of a country"
  putStrLn "4 - Display Population data of all the countries"
  putStrLn "5 - Display GDP data of all countries"
  putStrLn "6 - Exit"

  option <- prompt "\nSelect the option you want: "
  case option of
      "1" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulation countryName year
        nameToBeDetermined
      "2" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchGDP countryName year
        nameToBeDetermined
      "3" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulationAndGDP countryName year
        nameToBeDetermined
      "4" -> do
        displayAllPopulationData
        nameToBeDetermined
      "5" -> do
        displayAllGDPData
        nameToBeDetermined
      "6" -> die (show "Exitting..")
      _ -> do
        putStrLn "Invalid option selected. Please try again."
        nameToBeDetermined


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
  nameToBeDetermined
