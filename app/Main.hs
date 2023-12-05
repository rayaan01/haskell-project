
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
 , continent :: String
 , capital :: String
 }

data GDP = GDP
 { countryNameG :: String
 , totalPopulation :: Int
 , gdp :: Float
 }

instance Show Population where
   show population = mconcat [ show $ countryID population
                             , ".)  "
                             , countryNameP population
                             , ", "
                             , continent population
                             , ", "
                             , capital population]

instance Show GDP where
   show gdp = mconcat [ countryNameG gdp
                      , ".) "
                      , show $ totalPopulation gdp
                      , ", "
                      , show $ Main.gdp gdp
                      , "\n"]

addPopulation :: Int -> String -> String -> String -> IO ()
addPopulation countryID countryNameP continent capital = do
   conn <- open "tools.db"                                      
   execute conn "INSERT INTO POPULATION (countryID, countryNameP, continent, capital) VALUES (?,?,?,?)" (countryID, countryNameP, continent, capital)                                            
   print "Population data added"
   close conn  

addGDP :: String -> Int -> Float -> IO ()
addGDP countryNameG totalPopulation gdp = do
   conn <- open "tools.db"                                      
   execute conn "INSERT INTO GDP (countryNameG, totalPopulation, gdp) VALUES (?,?,?)" (countryNameG, totalPopulation, gdp)                                            
   print "GDP data added"
   close conn  

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn