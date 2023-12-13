module Fetch (downloadURLS ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import Types (URLS(..))

-- | Downloads both GDP and Population data from the URLs given.
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