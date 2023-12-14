module Main (main) where
import Fetch (downloadURLS, getURLConstructor)
import Database
import Types
import Parse (getGDP, getPOP)
import System.Exit (die)
import System.IO

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
        countryName <- initiateFuzzySearch
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulation countryName year
        nameToBeDetermined
      "2" -> do
        countryName <- initiateFuzzySearch
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchGDP countryName year
        nameToBeDetermined
      "3" -> do
        countryName <- initiateFuzzySearch
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

-- To Ensure the prompt is displayed before reading input
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

convertToString :: [CounOption] -> [String]
convertToString = map (\(CounOption str) -> str)

initiateFuzzySearch :: IO String
initiateFuzzySearch = do
    createFtsTable
    countryName <- prompt "\nEnter the country name: "
    results <- executeFuzzyMatch "tools.db" countryName
    -- let te =  "Select From one of these: " ++ (length)
    putStrLn "Select from list: \n"
    let gh = convertToString results
    mapM_ print gh

    option <-  prompt "\n Your option: "
    let op = read option :: Int

    return (gh !! (op - 1 ))
    -- year <- prompt "\nEnter the year (2010, 2015, or 2021): "
    -- fetchPopulationAndGDP capitalizedCountryName year
