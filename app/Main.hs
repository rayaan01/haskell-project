module Main (main) where
import Fetch (downloadURLS, getURLConstructor)
import Database
import Types
import Parse (getGDP, getPOP)
import System.Exit (die)
import System.IO
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust)


-- | main menu given to user for various operations 
loopForever = do
  putStrLn "\n\n1 - Fetch population of a country"
  putStrLn "2 - Fetch GDP of a country"
  putStrLn "3 - Fetch both Population and GDP of a country"
  putStrLn "4 - Display Population data of all the countries"
  putStrLn "5 - Display GDP data of all countries"
  putStrLn "6 - Update Population data of a country"
  putStrLn "7 - Update GDP data of a country"
  putStrLn "8 - Exit"

-- | Giving options for each use case
  option <- prompt "\nSelect the option you want: "
  case option of
      "1" -> do
        countryName <- initiateFuzzySearch
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulation countryName year
        loopForever
      "2" -> do
        countryName <- initiateFuzzySearch
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchGDP countryName year
        loopForever
      "3" -> do
        countryName <- initiateFuzzySearch
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        fetchPopulationAndGDP countryName year
        loopForever
      "4" -> do
        displayAllPopulationData
        loopForever
      "5" -> do
        displayAllGDPData
        loopForever
      "6" -> do
        countryName <- initiateFuzzySearch
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        new_value <- prompt "\nEnter new data (in millions): "
        updatePopulationData countryName year new_value
        loopForever
      "7" -> do
        countryName <- initiateFuzzySearch
        year <- prompt "\nEnter the year (2010, 2015, or 2021): "
        new_value <- prompt "\nEnter new data : "
        updateGDPData countryName year new_value
        loopForever
      "8" -> die (show "Exitting..")
      _ -> do
        putStrLn "Invalid option selected. Please try again."
        loopForever

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
  loopForever

-- To Ensure the prompt is displayed before reading input
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

convertToString :: [CounOption] -> [String]
convertToString = map (\(CounOption str) -> str)

-- | Implementing fuzzy seach for suggesting results to users with partial text input
initiateFuzzySearch :: IO String
initiateFuzzySearch = do
    createFtsTable
    countryName <- prompt "\nEnter the country name: "
    results <- executeFuzzyMatch dbPath countryName
    let gh = convertToString results

    if null gh then do
        putStrLn "Invalid country name. No matches found."

        loopForever
        return ""
    else do
        putStrLn "\nSelect from list: \n"
        mapM_ (\(i, name) -> putStrLn $ show i ++ " - " ++ name) $ zip [1..] gh

        optionStr <- prompt "\nYour option: "
        let maybeOp = readMaybe optionStr :: Maybe Int
        if isJust maybeOp && fromJust maybeOp > 0 && fromJust maybeOp <= length gh then
            return (gh !! (fromJust maybeOp - 1))
        else do
            putStrLn "\nInvalid option. Please try again."
            loopForever
            return ""
