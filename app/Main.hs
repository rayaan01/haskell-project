module Main (main) where

import Fetch (downloadFile)

data URLS = URLS {
  gdp :: String,
  pop :: String,
  isZip :: Bool
} deriving (Show)

main :: IO ()
main = do

  let dataUN = URLS {gdp = "https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv", pop = "https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv", isZip = True}
  
  let dataWorldBank = URLS {gdp = "https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv", pop = "https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=xml", isZip = False}
  downloadFile (gdp dataWorldBank) "gdp.zip"
  
