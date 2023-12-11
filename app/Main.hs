{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  let gdp_url = "https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=xml"
  responce <- httpBS gdp_url
  -- putStrLn $ show $ getResponseStatusCode responce
  B8.writeFile "gdp.zip" $ getResponseBody responce
  
