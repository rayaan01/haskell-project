{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

downloadZip :: String -> String -> IO ()
downloadZip url filename = do
  let req = parseRequest_ url
  responce <- httpBS req
  -- putStrLn $ show $ getResponseStatusCode responce
  B8.writeFile filename  $ getResponseBody responce
  

main :: IO ()
main = do
  let gdp_url = "https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=xml"
  downloadZip gdp_url "gdp.zip"
  -- responce <- httpBS gdp_url
  -- putStrLn $ show $ getResponseStatusCode responce
  -- B8.writeFile "gdp.zip" $ getResponseBody responce
  
