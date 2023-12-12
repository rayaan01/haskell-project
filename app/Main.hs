module Main (main) where
import Lib
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Typeable
import Data.Csv
import qualified Data.Vector as V

removeComma = filter (/= ',')

main :: IO ()
main = do
    csvData <- BL.readFile "new_gdp.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (country, year, gdp) -> do
            putStrLn("It's a country")