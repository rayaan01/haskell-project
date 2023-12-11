module Main (main) where

import Fetch (downloadZip)

main :: IO ()
main = do
  let gdp_url = "https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=xml"
  downloadZip gdp_url "gdp.zip"
  -- responce <- httpBS gdp_url
  -- putStrLn $ show $ getResponseStatusCode responce
  -- B8.writeFile "gdp.zip" $ getResponseBody responce
  
