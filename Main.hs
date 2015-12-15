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

-- =====  types =====

type Object = (String,[Double])

type Property = [Double]
type PropertiesCollection = M.Map Int Property
type ClasifyCollection = M.Map String PropertiesCollection

type ClassProperty = (Double, Double, Double)
type ClassPropertyCollection = M.Map Int ClassProperty
type Clasify = M.Map String ClassPropertyCollection

-- =====  bayes clasify traning =====

traningProperties :: [Property] -> PropertiesCollection
traningProperties = foldl trainingGroup M.empty . transpose
  where trainingGroup col prop = M.insert (M.size col) prop col

traningObjects :: [Object] -> ClasifyCollection
traningObjects = M.map traningProperties . foldl merge M.empty
  where merge m (k, xs) = M.insertWith (++) k [xs] m

calculateProperties :: Double ->Property -> ClassProperty
calculateProperties all prop = (avge, disp, per)
  where avge = (sum prop) / size
        disp = 1.0 / (size - 1) * disp_summ
        disp_summ = sum $ map (**2) $ map (\x -> x - avge) prop
        size = fromIntegral $ length prop
        per = size / all

training :: [Object] -> Clasify
training objs = M.map calculateClasses $ traningObjects objs
  where calculateClasses = M.map (\x -> calculateProperties size x)
        size = fromIntegral $ length objs

-- =====  bayes clasify testing =====

findClass :: Clasify -> [Double] -> String
findClass clasify obj = snd $ maximum $ map (\(x,y)  -> (y,x)) $ M.toList $ M.map findCoffs clasify
  where findCoffs prop_collection = (*) (persent prop_collection) $ product $ zipWith zipper obj $ M.elems prop_collection
        zipper prop (avge, disp, _) = 1 / sqrt(2 * pi * disp) * exp ((-1) * (prop - avge) ** 2 / 2 / disp)
        persent prop_collection = percentFor $ head $ M.elems prop_collection
        percentFor (_, _, x) = x

testingObject :: Clasify -> Object -> Bool
testingObject clasify (clas, prop) = clas == findClass clasify prop

testing :: Clasify -> [Object] -> Double
testing clasify objects = results / size
  where checkResult acc object = if testingObject clasify object then acc + 1 else acc
        results = foldl checkResult 0 objects
        size = fromIntegral $ length objects

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
