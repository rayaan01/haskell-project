module Fetch (downloadURLS, URLS(..)) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

data URLS = -- | URLS is used to save urls from which to download the data. 
  URLS {
  gdp :: String, -- ^ url from which to download GDP 
  pop :: String, -- ^ url from which to download pop
  isZip :: Bool  -- ^ bool to know if the link gives  a zip or not
} deriving (Show)

-- | dowloads both gdp and pop data from the urls given.
downloadURLS :: URLS -> IO ()
downloadURLS datasite = do
  let ext = if (isZip datasite ) then ".zip" else ".csv"
  downloadFile (gdp datasite) ("gdp" ++ ext)
  downloadFile (pop datasite) ("pop" ++ ext)

-- | downloads a file of an internet and saves the output to the given file.    
downloadFile :: String -> String -> IO ()
downloadFile url filename = do
  let req = parseRequest_ url
  responce <- httpBS req
  -- putStrLn $ show $ getResponseStatusCode responce
  B8.writeFile filename  $ getResponseBody responce

