
{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.SQLite.Simple                   
import Database.SQLite.Simple.FromRow           
import Data.Time    
import Types
import Data.Char (isDigit)   
import Text.Read (readMaybe)  
import System.IO (hFlush, stdout)
import Data.Char (toUpper, toLower)
import Data.List (intercalate)


withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn

addPopulation :: (Int, String, String, String, String, String) -> IO ()
addPopulation (countryID, countryNameP, capital, pop2010, pop2015, pop2021) = withConn "tools.db" $ \conn -> do
   execute conn "INSERT INTO POPULATION (countryID, countryNameP, capital, pop2010, pop2015, pop2021) VALUES (?,?,?,?,?,?)" (countryID, countryNameP, capital, pop2010, pop2015, pop2021)                                            

saveGDPData :: [RecordGDP] -> IO ()
saveGDPData gdpData = mapM_ (addGDP gdpData) gdpData

savePOPData :: [RecordPOP] -> IO ()
savePOPData popData = mapM_ (addPOP popData) popData

-- The addGDP function
getGdp :: String -> [RecordGDP] -> Int
getGdp yr records = 
    case filter (\r -> g_year r == yr) records of
        [] -> 0  -- or any other default value
        (x:_) -> read (filter isDigit (gdp x)) :: Int

addGDP :: [RecordGDP] -> RecordGDP -> IO ()
addGDP gdpData record = withConn "tools.db" $ \conn -> do
    let countryNameG = g_country record
    let countryRecords = filter (\r -> g_country r == countryNameG) gdpData
    let gdp2010 = getGdp "2010" countryRecords
    let gdp2015 = getGdp "2015" countryRecords
    let gdp2021 = getGdp "2021" countryRecords
    execute conn "INSERT OR REPLACE INTO GDP (countryNameG, gdp2010, gdp2015, gdp2021) VALUES (?,?,?,?)" (countryNameG, gdp2010, gdp2015, gdp2021)

addPOP :: [RecordPOP] -> RecordPOP -> IO ()
addPOP popData record = withConn "tools.db" $ \conn -> do
    let countryNameP = p_country record
    let countryID = p_id record
    let countryRecords = filter (\r -> p_country r == countryNameP) popData
    let pop2010 = getPop "2010" countryRecords
    let pop2015 = getPop "2015" countryRecords
    let pop2021 = getPop "2021" countryRecords
    execute conn "INSERT OR REPLACE INTO POPULATION (countryID, countryNameP, pop2010, pop2015, pop2021) VALUES (?,?,?,?,?)" (countryID, countryNameP, pop2010, pop2015, pop2021)

getPop :: String -> [RecordPOP] -> String
getPop yr records = 
    case filter (\r -> p_year r == yr) records of
        [] -> "0" 
        (x:_) -> ((pop x)) :: String

-- Use mapM_ to apply addPopulation and addGDP to each element in popData and gdpData
createTables :: IO ()
createTables = withConn "tools.db" $ \conn -> do
    execute_ conn "DROP TABLE IF EXISTS POPULATION;"
    execute_ conn "DROP TABLE IF EXISTS GDP;"
    execute_ conn "CREATE TABLE POPULATION (countryID INTEGER PRIMARY KEY, countryNameP TEXT, capital TEXT, pop2010 TEXT, pop2015 TEXT, pop2021 TEXT);"
    execute_ conn "CREATE TABLE GDP (countryNameG TEXT PRIMARY KEY, gdp2010 FLOAT, gdp2015 FLOAT, gdp2021 FLOAT, FOREIGN KEY (countryNameG) REFERENCES POPULATION(countryNameP));"
    putStrLn "Tables created"

-- Use mapM_ to apply addPopulation and addGDP to each element in popData and gdpData

fetchGDP :: String -> String -> IO ()
fetchGDP countryName year = withConn "tools.db" $ \conn -> do
    r <- query conn "SELECT gdp2010, gdp2015, gdp2021 FROM GDP WHERE countryNameG = ?" (Only countryName) :: IO [(Float, Float, Float)]
    case r of
        [] -> putStrLn "No data found"
        [(gdp2010, gdp2015, gdp2021)] -> case year of
            "2010" -> putStrLn ("GDP Data of "++ countryName ++ " :" ++ show gdp2010 ++"\n")
            "2015" -> putStrLn ("GDP Data of "++ countryName ++ " :" ++ show gdp2015 ++"\n")
            "2021" -> putStrLn ("GDP Data of "++ countryName ++ " :" ++ show gdp2021 ++"\n")
            _ -> putStrLn "Invalid year"

fetchPopulation :: String -> String -> IO ()
fetchPopulation countryName year = withConn "tools.db" $ \conn -> do
    r <- query conn "SELECT pop2010, pop2015, pop2021 FROM POPULATION WHERE countryNameP = ?" (Only countryName) :: IO [(String, String, String)]
    case r of
        [] -> putStrLn "No data found"
        [(pop2010, pop2015, pop2021)] -> case year of
            "2010" -> putStrLn ("\n\nPolution Data of "++ countryName ++ " : " ++ pop2010 ++ " Millions\n")
            "2015" -> putStrLn ("\n\nPolution Data of "++ countryName ++ " : " ++ pop2015 ++ " Millions\n")
            "2021" -> putStrLn ("\n\nPolution Data of "++ countryName ++ " : " ++ pop2021 ++ " Millions\n")
            _ -> putStrLn "Invalid year"

--------------------------------------

-- selectYear y 
--    | y == "2010" = 2010
--    | y == "2015" = 2015
--    | y == "2021" = 2021
--    | otherwise = 0

-- Function to prompt user and fetch data
fetchData :: IO ()
fetchData = do
    countryName <- prompt "\nEnter the country name: "
    let capitalizedCountryName = capitalizeWords countryName
    year <- prompt "\nEnter the year (2010, 2015, or 2021): "
    fetchPopulationAndGDP capitalizedCountryName year

capitalizeWords :: String -> String
capitalizeWords = intercalate " " . map capitalizeWord . words

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : map toLower xs

-- To Ensure the prompt is displayed before reading input
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout  
    getLine

-- Function to fetch population and GDP data
fetchPopulationAndGDP :: String -> String -> IO ()
fetchPopulationAndGDP countryName year = withConn "tools.db" $ \conn -> do
    popResult <- query conn "SELECT pop2010, pop2015, pop2021 FROM POPULATION WHERE countryNameP = ?" (Only countryName) :: IO [(String, String, String)]
    case popResult of
        [] -> putStrLn "No population data found"
        [(pop2010, pop2015, pop2021)] -> putStrLn $ formatPopulationData year countryName pop2010 pop2015 pop2021
        _ -> putStrLn "Error: Multiple population records found"

    gdpResult <- query conn "SELECT gdp2010, gdp2015, gdp2021 FROM GDP WHERE countryNameG = ?" (Only countryName) :: IO [(Float, Float, Float)]
    case gdpResult of
        [] -> putStrLn "No GDP data found"
        [(gdp2010, gdp2015, gdp2021)] -> putStrLn $ formatGDPData year countryName gdp2010 gdp2015 gdp2021
        _ -> putStrLn "Error: Multiple GDP records found"

-- Function to format population data for display
formatPopulationData :: String -> String -> String -> String -> String -> String
formatPopulationData year countryName pop2010 pop2015 pop2021 =
    let population = case year of
            "2010" -> pop2010
            "2015" -> pop2015
            "2021" -> pop2021
            _ -> "Invalid year"
    in "\nPopulation Data of " ++ countryName ++ " for " ++ year ++ ": " ++ population ++ " Millions\n"

-- Function to format GDP data for display
formatGDPData :: String -> String -> Float -> Float -> Float -> String
formatGDPData year countryName gdp2010 gdp2015 gdp2021 =
    let gdp = case year of
            "2010" -> gdp2010
            "2015" -> gdp2015
            "2021" -> gdp2021
            _ -> error "Invalid year"
    in "\nGDP Data of " ++ countryName ++ " for " ++ year ++ ": " ++ show gdp ++ "\n"
