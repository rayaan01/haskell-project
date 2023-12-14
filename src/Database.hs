
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
 , pop2010 :: Int
 , pop2015 :: Int
 , pop2021 :: Int
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

-- Assuming popData and gdpData are lists of tuples...
popData :: [(Int, String, String, Int, Int, Int)]
popData = [(3, "USA", "Delhi", 100, 200, 300), (4, "Russia", "Karachi", 100, 200, 300)]

addPopulation :: (Int, String, String, Int, Int, Int) -> IO ()
addPopulation (countryID, countryNameP, capital, pop2010, pop2015, pop2021) = withConn "tools.db" $ \conn -> do
   execute conn "INSERT INTO POPULATION (countryID, countryNameP, capital, pop2010, pop2015, pop2021) VALUES (?,?,?,?,?,?)" (countryID, countryNameP, capital, pop2010, pop2015, pop2021)                                            


callMain :: [RecordGDP] -> IO ()
callMain gdpData = mapM_ (addGDP gdpData) gdpData

-- The addGDP function
getGdp :: String -> [RecordGDP] -> Int
getGdp yr records = 
    case filter (\r -> year r == yr) records of
        [] -> 0  -- or any other default value
        (x:_) -> read (filter isDigit (gdp x)) :: Int

addGDP :: [RecordGDP] -> RecordGDP -> IO ()
addGDP gdpData record = withConn "tools.db" $ \conn -> do
    let countryNameG = r_country record
    let countryRecords = filter (\r -> r_country r == countryNameG) gdpData
    let gdp2010 = getGdp "2010" countryRecords
    let gdp2015 = getGdp "2015" countryRecords
    let gdp2021 = getGdp "2021" countryRecords
    execute conn "INSERT OR REPLACE INTO GDP (countryNameG, gdp2010, gdp2015, gdp2021) VALUES (?,?,?,?)" (countryNameG, gdp2010, gdp2015, gdp2021)




-- Use mapM_ to apply addPopulation and addGDP to each element in popData and gdpData
createTables :: IO ()
createTables = withConn "tools.db" $ \conn -> do
    execute_ conn "DROP TABLE IF EXISTS POPULATION;"
    execute_ conn "DROP TABLE IF EXISTS GDP;"
    execute_ conn "CREATE TABLE POPULATION (countryID INTEGER PRIMARY KEY, countryNameP TEXT, capital TEXT, pop2010 INTEGER, pop2015 INTEGER, pop2021 INTEGER);"
    execute_ conn "CREATE TABLE GDP (countryNameG TEXT PRIMARY KEY, gdp2010 FLOAT, gdp2015 FLOAT, gdp2021 FLOAT);"
    putStrLn "Tables created"

-- Use mapM_ to apply addPopulation and addGDP to each element in popData and gdpData

   
