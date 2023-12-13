module Main (main) where

import Fetch (downloadURLS, URLS(..))
import Parse

-- | The Main function
main :: IO ()
main = do
  -- Fetching
  let dataUN = URLS {gdp = "https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv", pop = "https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv", isZip = False}
  
  let dataWorldBank = URLS {gdp = "https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=csv", pop = "https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv", isZip = True}

  downloadURLS dataUN

  -- Parsing

  parseCSV "gdp.csv"
