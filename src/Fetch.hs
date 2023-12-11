module Fetch (downloadURLS, URLS(..)) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

data URLS = URLS {
  gdp :: String,
  pop :: String,
  isZip :: Bool
} deriving (Show)

downloadURLS :: URLS -> IO ()
downloadURLS datasite = do
  let ext = if (isZip datasite ) then ".zip" else ".csv"
  downloadFile (gdp datasite) ("gdp" ++ ext)
  downloadFile (pop datasite) ("pop" ++ ext)
    
downloadFile :: String -> String -> IO ()
downloadFile url filename = do
  let req = parseRequest_ url
  responce <- httpBS req
  -- putStrLn $ show $ getResponseStatusCode responce
  B8.writeFile filename  $ getResponseBody responce

