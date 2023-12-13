module Main (main) where

import Fetch 
import Parse
import Types
import Database

-- | The Main function
main :: IO ()
main = do
  -- Fetching
  let dataUN = URLS {gdp_url = "https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv", pop_url = "https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv", isZip = False}
  
  let dataWorldBank = URLS {gdp_url = "https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=csv", pop_url = "https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv", isZip = True}

  -- _ = if not $ checkURLS dataUN then return () else True

  -- let files = CSVFiles { gdpf "gdp", popf "pop"}

  downloadURLS dataUN

  -- Parsing

  parseCSV "gdp.csv"

  -- Database

  createTables
  mapM_ addPopulation popData
  mapM_ addGDP gdpData
  putStrLn "Data Added Succesfully!"
