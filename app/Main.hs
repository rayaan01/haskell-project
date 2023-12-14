module Main (main) where
import Fetch (downloadURLS, getURLConstructor) 
import Database
import Parse (getGDP, getPOP)

nameToBeDetermined = do
  putStrLn("\n\n1 - Fetch population of a country")
  putStrLn("2 - Fetch GDP of a country")
  putStrLn("3 - Fetch both Population and GDP of a country")
  putStrLn("4 - Display Population data of all the countries")
  putStrLn("5 - Display GDP data of all countries")

  option <- prompt "\nSelect the option you want: "
  case option of
      "1" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulation countryName year
      "2" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchGDP countryName year
      "3" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulationAndGDP countryName year
      -- "4" -> displayAllPopulationData
      -- "5" -> displayAllGDPData
      _ -> nameToBeDetermined
      

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

  putStrLn("\n\n1 - Fetch population of a country")
  putStrLn("2 - Fetch GDP of a country")
  putStrLn("3 - Fetch both Population and GDP of a country")
  putStrLn("4 - Display Population data of all the countries")
  putStrLn("5 - Display GDP data of all countries")

  option <- prompt "\nSelect the option you want: "
  case option of
      "1" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulation countryName year
      "2" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchGDP countryName year
      "3" -> do
        countryName <- prompt "\nEnter the country name: "
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulationAndGDP countryName year
      -- "4" -> displayAllPopulationData
      -- "5" -> displayAllGDPData
      _ -> putStrLn "Invalid option selected. Please try again."
