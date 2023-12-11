module Main (main) where

import Lib
import qualified Data.ByteString.Lazy as BL
import Data.Typeable
import Control.Monad.State
import Data.Csv
import qualified Data.Vector as V

removeComma = filter (/= ',')

data MyState = MyState { startParsing :: Bool }

updateState :: Bool -> State MyState ()
updateState newValue = modify (\s -> s { startParsing = newValue })

useState :: State MyState String
useState = do
    value <- gets startParsing
    return $ "Start Parsing value: " ++ show value

main :: IO ()
main = do
    csvData <- BL.readFile "new_gdp.csv"
    let initialState = MyState { startParsing = False }
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (country, year, gdp) -> do
            let (result, finalState) = runState(do
                if country == "Afghanistan"
                    then updateState True
                    else updateState False

                    result <- useState
                    return result
                    ) initialState


            -- if (startParsing)
            --     then do
            --         putStrLn("New Country")
            --     else
            --         putStrLn("Not a country")

            let noCommaGDP = removeComma gdp
            let intGDP = read noCommaGDP :: Int
            putStrLn ("The country is " ++ country ++ "\nThe year is " ++ year ++ "\nThe GDP " ++ noCommaGDP ++ "\nInt " ++ show intGDP)
            putStrLn ("Type of country: " ++ show (typeOf country))
            putStrLn ("Type of year: " ++ show (typeOf year))
            putStrLn ("Type of gdp: " ++ show (typeOf intGDP))
