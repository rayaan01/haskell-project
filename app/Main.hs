{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Database.SQLite.Simple                   
import Database.SQLite.Simple.FromRow           
import Data.Time                                

main :: IO ()
main = print "db-lesson"

-- | Represent population data with country Id(Primary Key), country name(foreign key) and population data for years 2010, 2015, 2021.
data Population = Population
 { countryID :: Int
 , countryNameP :: String
 , pop2010 :: Int
 , pop2015 :: Int
 , pop2021 :: Int
 , capital :: String
 }

-- | Represent GDP data with country name(foreign key) and GDP data for years 2010, 2015, 2021.
data GDP = GDP
 { countryNameG :: String
 , gdp2010 :: Int
 , gdp2015 :: Int
 , gdp2021 :: Int
 }

-- | To show headers to the population datatable  
instance Show Population where
   show population = mconcat [ show $ countryID population
                             , ".) "
                             , countryNameP population
                             , ", Capital: "
                             , capital population
                             , ", Population in 2010: "
                             , show $ pop2010 population
                             , ", 2015: "
                             , show $ pop2015 population
                             , ", 2021: "
                             , show $ pop2021 population
                             , "\n"]

-- | To show headers to GDP datatable. 
instance Show GDP where
   show gdp = mconcat [ countryNameG gdp
                      , ", GDP in 2010: "
                      , show $ gdp2010 gdp
                      , ", 2015: "
                      , show $ gdp2015 gdp
                      , ", 2021: "
                      , show $ gdp2021 gdp
                      , "\n"]

-- | Adds population data to the database
addPopulation :: Int -> String -> String -> Int -> Int -> Int -> IO ()
addPopulation countryID countryNameP capital pop2010 pop2015 pop2021 = withConn "tools.db" $ \conn -> do
   execute conn "INSERT INTO POPULATION (countryID, countryName, capital, 2010, 2015, 2021) VALUES (?,?,?,?,?,?)" (countryID, countryNameP, capital, pop2010, pop2015, pop2021)                                            
   putStrLn "Population data added"

-- | Adds GDP data to the database
addGDP :: String -> Int -> Int -> Int -> IO ()
addGDP countryNameG gdp2010 gdp2015 gdp2021 = withConn "tools.db" $ \conn -> do
   execute conn "INSERT INTO GDP (countryName, 2010, 2015, 2021) VALUES (?,?,?,?)" (countryNameG, gdp2010, gdp2015, gdp2021)                                            
   putStrLn "GDP data added"

-- | Helper function to manage database connections.
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn
