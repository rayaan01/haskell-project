module Types (URLS(..), CSVFiles(..)) where

data URLS = -- | URLS is used to save urls from which to download the data. 
  URLS {
  gdp :: String, -- ^ url from which to download GDP 
  pop :: String, -- ^ url from which to download pop
  isZip :: Bool  -- ^ bool to know if the link gives  a zip or not
} deriving (Show)

data CSVFiles = -- | Used to store file names of the files
  CSVFiles {
    gdpf :: String,
    popf :: String
  }
