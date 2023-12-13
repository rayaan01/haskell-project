module Fetch (downloadURLS ) where

import Network.HTTP.Simple
import Network.HTTP.Types.Status
import qualified Data.ByteString.Char8 as B8
import Types (URLS(..))

writeToFile contents filename = B8.writeFile filename contents  

-- | dowloads both gdp and pop data from the urls given.
downloadURLS :: URLS -> IO ()
downloadURLS datasite = do
  let ext = if isZip datasite then ".zip" else ".csv"
  
  contents <- downloadContent (gdp_url datasite) 
  B8.writeFile ("gdp" ++ ext) contents

  popContent <- downloadContent (pop_url datasite)
  B8.writeFile ("pop" ++ ext) popContent

-- | downloads a file of an internet and saves the output to the given file.    
downloadContent :: String -> IO B8.ByteString 
downloadContent url = do
  let req = parseRequest_ url
  responce <- httpBS req
  pure $ getResponseBody responce

