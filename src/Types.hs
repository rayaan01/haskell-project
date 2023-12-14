module Types (URLS(..), CSVFiles(..), RecordGDP(..), RecordPOP(..)) where

data URLS = -- | URLS is used to save urls from which to download the data. 
  URLS {
  gdp_url :: String, -- ^ url from which to download GDP 
  pop_url :: String, -- ^ url from which to download pop
  isZip :: Bool  -- ^ bool to know if the link gives  a zip or not
} deriving (Show)

data CSVFiles = -- | Used to store file names of the files
  CSVFiles {
    gdpf :: String,
    popf :: String
  }

data RecordGDP = RecordGDP {
  g_id :: Int,
  g_country :: String,
  g_year :: String,
  gdp :: String
} deriving (Show)

data RecordPOP = RecordPOP {
  p_id :: Int,
  p_country :: String,
  p_year :: String,
  pop :: String
} deriving (Show)
