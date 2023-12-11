module Fetch (downloadFile) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

downloadFile :: String -> String -> IO ()
downloadFile url filename = do
  let req = parseRequest_ url
  responce <- httpBS req
  -- putStrLn $ show $ getResponseStatusCode responce
  B8.writeFile filename  $ getResponseBody responce

