
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Database.SQLite.Simple                   
import Database.SQLite.Simple.FromRow           
import Data.Time                                

main :: IO ()
main = print "db-lesson"

data Population = Population
 { countryID :: Int
 , countryNameP :: String
 , pop2005 :: Int
 , pop2010 :: Int
 , pop2015 :: Int
 , pop2020 :: Int
 , capital :: String
 }

data GDP = GDP
 { countryNameG :: String
 , gdp2005 :: Int
 , gdp2010 :: Int
 , gdp2015 :: Int
 , gdp2020 :: Int
 }

instance Show Population where
   show population = mconcat [ show $ countryID population
                             , ".) "
                             , countryNameP population
                             , ", Capital: "
                             , capital population
                             , ", Population in 2005: "
                             , show $ pop2005 population
                             , ", 2010: "
                             , show $ pop2010 population
                             , ", 2015: "
                             , show $ pop2015 population
                             , ", 2020: "
                             , show $ pop2020 population
                             , "\n"]

instance Show GDP where
   show gdp = mconcat [ countryNameG gdp
                      , ", GDP in 2005: "
                      , show $ gdp2005 gdp
                      , ", 2010: "
                      , show $ gdp2010 gdp
                      , ", 2015: "
                      , show $ gdp2015 gdp
                      , ", 2020: "
                      , show $ gdp2020 gdp
                      , "\n"]


addPopulation :: Int -> String -> String -> Int -> Int -> Int -> Int -> IO ()
addPopulation countryID countryNameP capital pop2005 pop2010 pop2015 pop2020 = withConn "tools.db" $ \conn -> do
   execute conn "INSERT INTO POPULATION (countryID, countryName, capital, 2005, 2010, 2015, 2020) VALUES (?,?,?,?,?,?,?)" (countryID, countryNameP, capital, pop2005, pop2010, pop2015, pop2020)                                            
   putStrLn "Population data added"

addGDP :: String -> Int -> Int -> Int -> Int -> IO ()
addGDP countryNameG gdp2005 gdp2010 gdp2015 gdp2020 = withConn "tools.db" $ \conn -> do
   execute conn "INSERT INTO GDP (countryName, 2005, 2010, 2015, 2020) VALUES (?,?,?,?,?)" (countryNameG, gdp2005, gdp2010, gdp2015, gdp2020)                                            
   putStrLn "GDP data added"

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn
