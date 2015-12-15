{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Numeric
import Prelude
import Data.List
import System.IO
import Data.Maybe
import Data.CSV.Conduit
import Data.Conduit
import System.Console.CmdArgs

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

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

convertFromCsv :: InputConfigs -> V.Vector (Row String) -> [Object]
convertFromCsv configs = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow . skipHeader
          processRow row = (last row, processDoubles(init row))
          processDoubles = map (fromMaybe 0.0) . filter isJust . map maybeRead . filterEmpty . skipNumber
          filterEmpty = filter (\s -> T.strip (T.pack s) /= T.pack "")
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])
          skipHeader = if header configs then tail else id
          skipNumber = if rowNumber configs then tail else id

-- =====  output builder  =====

convertClasify :: Clasify -> [String]
convertClasify clasify = map (\x -> concat (showClass x)) $ M.toList clasify
  where showClass (clas, prop) = (:) clas $ map showProp $ M.toList prop
        showProp (index, values) = " - " ++ show (index + 1) ++ "(" ++ (showValues values) ++ ")"
        showValues (a,b,_) =  (take 6 $ show a) ++ ";" ++ (take 6 $ show b)

convertToCsv :: Clasify -> [B.ByteString]
convertToCsv = intersperse (BS.pack [13, 10]) . map B.pack . convertClasify

buildOutputHandle :: InputConfigs -> IO Handle
buildOutputHandle configs
    | outputFile configs /= ""  = openFile (outputFile configs) WriteMode
    | otherwise            = return stdout

-- =====  cmd args  =====

data InputConfigs = InputConfigs {
  delemiter :: String
  ,inputFile :: FilePath
  ,outputFile :: FilePath
  ,header :: Bool
  ,rowNumber :: Bool
  } deriving (Show, Data, Typeable)

defaultInputConfigs = InputConfigs {
  delemiter = ","                         &= help "Csv delemiter"
  ,inputFile = "./examples/butterfly.txt" &= help "Input file name"
  ,outputFile = ""                        &= help "Output file name (default console)"
  ,header = False                         &= help "Have csv header?"
  ,rowNumber = False                      &= help "Have csv number (row's head)?"
}  &= summary "Lab2 Bayes 2015" &= program "lab2"

main :: IO ()
main = do
  configs <- cmdArgs defaultInputConfigs
  let csvOpts = defCSVSettings {csvSep = head(delemiter configs), csvQuoteChar = Nothing}

  input <- runResourceT $ readCSVFile csvOpts $ inputFile configs
  let objects = convertFromCsv configs input
  let result = training objects

  runResourceT $ CL.sourceList (convertToCsv result) $$ CB.sinkIOHandle (buildOutputHandle configs)
