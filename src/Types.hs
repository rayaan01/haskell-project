{-# LANGUAGE GADTs #-}
-- | This file has all data types used throughout the application.
module Types (URLS(..), CSVFiles(..), RecordGDP(..), RecordPOP(..), Population(..), GDP(..), CounOption(..)) where
import Database.SQLite.Simple.FromRow

-- | URLS is used to save urls from which to download the data.
data URLS =  
  URLS {
  gdp_url :: String, -- ^ url from which to download GDP 
  pop_url :: String, -- ^ url from which to download pop
  isZip :: Bool  -- ^ bool to know if the link gives  a zip or not
} deriving (Show)

-- | Used to store file names of the files
data CSVFiles = 
  CSVFiles {
    gdpf :: String,
    popf :: String
  }

-- | recordGDP represents a single record of GDP data.
data RecordGDP = RecordGDP {
  g_id :: Int,
  g_country :: String,
  g_year :: String,
  gdp :: String
} deriving (Show)

-- | recordPOP represents a single record for POP data.
data RecordPOP = RecordPOP {
  p_id :: Int,
  p_country :: String,
  p_year :: String,
  pop :: String
} deriving (Show)

-- | The structure of population data.
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

-- | The structure of GDP data.
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

data CounOption where
  CounOption :: String -> CounOption
  deriving (Show)

instance FromRow CounOption where
  fromRow = CounOption <$> field
