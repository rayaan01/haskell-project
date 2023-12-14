module Fetch (downloadURLS, getURLConstructor) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import Types
-- | Downloads both GDP and Population data from the URLs given.
getURLConstructor :: String -> URLS
getURLConstructor source =
  if source == "UN" 
    then URLS { 
    gdp_url = "https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv",
    pop_url = "https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv",
    isZip = False 
  }
    else URLS { 
    gdp_url = "https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=csv",
    pop_url = "https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv",
    isZip = True
  }

-- | Downloads both GDP and Population data from the URLs given.
-- | Data from UN comes in a CSV format. Whereas from the World Bank, it is a zipped csv.
downloadURLS :: URLS -> IO ()
downloadURLS datasite = do
  let ext = if isZip datasite then ".zip" else ".csv"
  
  contents <- downloadContent (gdp_url datasite)
  B8.writeFile ("gdp" ++ ext) contents

  popContent <- downloadContent (pop_url datasite)
  B8.writeFile ("pop" ++ ext) popContent

-- | Downloads a file from the URLs and saves the output.
downloadContent :: String -> IO B8.ByteString 

downloadContent url = do
  let req = parseRequest_ url
  response <- httpBS req
  pure $ getResponseBody response