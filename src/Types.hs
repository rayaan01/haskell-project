module Types (URLS(..), CSVFiles(..), RecordGDP(..), RecordPOP(..), Population(..), GDP(..)) where

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

data Population = Population
 { countryID :: Int
 , countryNameP :: String
 , pop2010 :: Int
 , pop2015 :: Int
 , pop2021 :: Int
 , capital :: String
 }

instance Show Population where
   show population = mconcat [ show $ countryID population
                             , ".) "
                             , countryNameP population
                             , ", Capital: "
                             , capital population
                             , ", 2010: "
                             , show $ pop2010 population
                             , ", 2015: "
                             , show $ pop2015 population
                             , ", 2021: "
                             , show $ pop2021 population
                             , "\n"]

data GDP = GDP
 { countryNameG :: String
 , gdp2010 :: Int
 , gdp2015 :: Int
 , gdp2021 :: Int
 }

instance Show GDP where
   show gdp = mconcat [ countryNameG gdp
                      , ", 2010: "
                      , show $ gdp2010 gdp
                      , ", 2015: "
                      , show $ gdp2015 gdp
                      , ", 2021: "
                      , show $ gdp2021 gdp
                      , "\n"]
