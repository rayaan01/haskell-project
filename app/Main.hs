module Main (main) where

import Fetch (downloadURLS, URLS(..))

main :: IO ()
main = do

  let dataUN = URLS {gdp = "https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv", pop = "https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv", isZip = False}
  
  let dataWorldBank = URLS {gdp = "https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv", pop = "https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=xml", isZip = True}

  downloadURLS dataWorldBank 
  
