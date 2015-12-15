module Main where

import Prelude
import Data.List
import System.IO
import Data.Maybe
import Data.CSV.Conduit
import Data.Conduit

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.Map as M

type Object = (String,[Double])

type Property = [Double]
type PropertiesCollection = M.Map Int Property
type ClasifyCollection = M.Map String PropertiesCollection

type ClassProperty = (Double, Double)
type ClassPropertyCollection = M.Map Int ClassProperty
type Clasify = M.Map String ClassPropertyCollection

-- =====  bayes clasify =====

traningProperties :: [Property] -> PropertiesCollection
traningProperties = foldl trainingGroup M.empty . transpose
  where trainingGroup col prop = M.insert (M.size col) prop col

traningObjects :: [Object] -> ClasifyCollection
traningObjects = M.map traningProperties . foldl merge M.empty
  where merge m (k, xs) = M.insertWith (++) k [xs] m

calculateProperties :: Property -> ClassProperty
calculateProperties prop = (avge, disp)
  where avge = (sum prop) / size
        disp = 1.0 / (size - 1) * disp_summ
        disp_summ = sum $ map (**2) $ map (\x -> x - avge) prop
        size = fromIntegral $ length prop

training :: [Object] -> Clasify
training = M.map calculateClasses . traningObjects
  where calculateClasses = M.map calculateProperties

-- =====  input parse  =====

convertFromCsv :: V.Vector (Row String) -> [Object]
convertFromCsv = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow
          processRow row = (last row, processDoubles(init row))
          processDoubles = map (fromMaybe 0.0) . filter isJust . map maybeRead
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])

main :: IO ()
main = do
  let csvOpts = defCSVSettings {csvSep = ',', csvQuoteChar = Nothing}

  input <- runResourceT $ readCSVFile csvOpts "./examples/butterfly.txt"

  print $ convertFromCsv input
