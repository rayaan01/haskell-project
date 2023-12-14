
{-# LANGUAGE OverloadedStrings #-}
module Database where

import Control.Applicative
import Database.SQLite.Simple                   
import Database.SQLite.Simple.FromRow           
import Data.Time    
import Types
import Data.Char (isDigit)                            

data Population = Population
 { countryID :: Int
 , countryNameP :: String
 , pop2010 :: String
 , pop2015 :: String
 , pop2021 :: String
 , capital :: String
 }

data GDP = GDP
 { countryNameG :: String
 , gdp2010 :: Int
 , gdp2015 :: Int
 , gdp2021 :: Int
 }

instance Show Population where
   show population = mconcat [ show $ countryID population
                             , ".) "
                             , countryNameP population
                             , ", Capital: "
                             , capital population
                             , ", 2010: "
                             , show $ pop2010 population
                             , ", 2015: "
                             , show $ pop2015 population
                             , ", 2021: "
                             , show $ pop2021 population
                             , "\n"]

instance Show GDP where
   show gdp = mconcat [ countryNameG gdp
                      , ", 2010: "
                      , show $ gdp2010 gdp
                      , ", 2015: "
                      , show $ gdp2015 gdp
                      , ", 2021: "
                      , show $ gdp2021 gdp
                      , "\n"]

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
    let countryRecords = filter (\r -> p_country r == countryNameP) popData
    let pop2010 = getPop "2010" countryRecords
    let pop2015 = getPop "2015" countryRecords
    let pop2021 = getPop "2021" countryRecords
    execute conn "INSERT OR REPLACE INTO POPULATION (countryNameP, pop2010, pop2015, pop2021) VALUES (?,?,?,?)" (countryNameP, pop2010, pop2015, pop2021)

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
    execute_ conn "CREATE TABLE POPULATION (countryNameP TEXT PRIMARY KEY, capital TEXT, pop2010 TEXT, pop2015 TEXT, pop2021 TEXT);"
    execute_ conn "CREATE TABLE GDP (countryNameG TEXT PRIMARY KEY, gdp2010 FLOAT, gdp2015 FLOAT, gdp2021 FLOAT);"
    putStrLn "Tables created"

-- Use mapM_ to apply addPopulation and addGDP to each element in popData and gdpData

fetchGDP :: String -> String -> IO ()
fetchGDP countryName year = withConn "tools.db" $ \conn -> do
    r <- query conn "SELECT gdp2010, gdp2015, gdp2021 FROM GDP WHERE countryNameG = ?" (Only countryName) :: IO [(Float, Float, Float)]
    case r of
        [] -> putStrLn "No data found"
        [(gdp2010, gdp2015, gdp2021)] -> case year of
            "2010" -> print (countryName, gdp2010)
            "2015" -> print (countryName, gdp2015)
            "2021" -> print (countryName, gdp2021)
            _ -> putStrLn "Invalid year"

fetchPopulation :: String -> String -> IO ()
fetchPopulation countryName year = withConn "tools.db" $ \conn -> do
    r <- query conn "SELECT pop2010, pop2015, pop2021 FROM POPULATION WHERE countryNameP = ?" (Only countryName) :: IO [(String, String, String)]
    case r of
        [] -> putStrLn "No data found"
        [(pop2010, pop2015, pop2021)] -> case year of
            "2010" -> print (countryName, pop2010 ++ " Millions")
            "2015" -> print (countryName, pop2015 ++ " Millions")
            "2021" -> print (countryName, pop2021 ++ " Millions")
            _ -> putStrLn "Invalid year"
