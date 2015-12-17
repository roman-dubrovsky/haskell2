{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Numeric
import Prelude
import Data.List
import Data.List.Split
import System.IO
import Data.Maybe
import Data.Conduit
import Control.Monad.Trans.Resource
import System.Console.CmdArgs
import Control.Monad.State
import Control.Monad.Par
import System.Random

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
  where calculateClasses = M.fromList . runPar . parMap (\(a, x) -> (a, calculateProperties size x)) . M.toList
        size = fromIntegral $ length objs

-- =====  bayes clasify testing =====

findClass :: Clasify -> [Double] -> String
findClass clasify obj = snd $ maximum $ map (\(x,y)  -> (y,x)) $ M.toList $ M.map findCoffs clasify
  where findCoffs prop_collection = (*) (persent prop_collection) $ product $ zipWith zipper obj $ M.elems prop_collection
        zipper prop (avge, disp, _) = 1 / sqrt(2 * pi * disp) * exp ((-1) * (prop - avge) ** 2 / 2 / disp)
        persent prop_collection = percentFor $ head $ M.elems prop_collection
        percentFor (_, _, x) = x

testingObject :: Clasify -> Object -> Double
testingObject clasify (clas, prop) = if (clas == findClass clasify prop) then 1 else 0

testing :: Clasify -> [Object] -> Double
testing clasify objects = results / size
  where results = sum $ runPar $ parMap (testingObject clasify) objects
        size = fromIntegral $ length objects


-- =====  state part =====

randomizeObjects :: [Object] -> Int -> StdGen -> ([Object], [Object])

randomizeObjects [] _ _ = ([], [])
randomizeObjects (x:xs) l rand
  | l' < l = (x:x1, x2)
  | otherwise = (x1, x:x2)
  where (x1, x2) = randomizeObjects xs l new_rand
        l' = head $ randomRs (0, 100) $ rand
        new_rand = snd (System.Random.split rand)

type CState = (Double, Clasify)

clasifyState :: (Int, [Object], StdGen, Int) -> State CState Clasify
clasifyState (0, _, _, _) = do
  (_, cl) <- Control.Monad.State.get
  return cl

clasifyState (n, objs, rand, t) = do
  (val, cl) <- Control.Monad.State.get

  let (learn, test) = randomizeObjects objs t rand
  let clasify = training learn
  let cof = testing clasify test
  let isNew = cof > val

  case isNew of
      True          -> Control.Monad.State.put (cof, clasify)
      _             -> Control.Monad.State.put (val, cl)

  clasifyState (n-1, objs, snd (System.Random.split rand), t)

-- =====  input parse  =====

convertFromCsv :: InputConfigs -> V.Vector ([String]) -> [Object]
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
convertToCsv = map (\x -> B.append x newLine) . map B.pack . convertClasify
  where newLine = BS.pack [13, 10]

buildOutputHandle :: InputConfigs -> IO Handle
buildOutputHandle configs
    | outputFile configs /= ""  = openFile (outputFile configs) WriteMode
    | otherwise            = return stdout

-- =====  conduit function =====

sourse :: String -> Source IO String
sourse filepath = do
  handle <- liftIO $ openFile filepath ReadMode
  loopRead handle
  where
    loopRead handle = do
      eof <- liftIO $ hIsEOF handle
      if eof
        then return ()
        else do
          c <- liftIO $ hGetLine handle
          yield c
          loopRead handle

parseString :: String -> Conduit String IO [String]
parseString delimetr = do
  str <- await
  case str of
    Nothing -> return ()
    Just s -> do
      yield $ splitOn delimetr s
      parseString delimetr

-- =====  cmd args  =====

data InputConfigs = InputConfigs {
  delemiter :: String
  ,inputFile :: FilePath
  ,outputFile :: FilePath
  ,header :: Bool
  ,rowNumber :: Bool
  ,number :: Int
  ,percent :: Int
  } deriving (Show, Data, Typeable)

defaultInputConfigs = InputConfigs {
  delemiter = ","                         &= help "Csv delemiter"
  ,inputFile = "./examples/irises.txt"    &= help "Input file name"
  ,outputFile = ""                        &= help "Output file name (default console)"
  ,header = False                         &= help "Have csv header?"
  ,rowNumber = False                      &= help "Have csv number (row's head)?"
  ,number = 5                             &= help "Numbers count for studing"
  ,percent = 50                           &= help "Percent on learning objects"
}  &= summary "Lab2 Bayes 2015" &= program "lab2"

main :: IO ()
main = do
  configs <- cmdArgs defaultInputConfigs
  rand <- newStdGen

  let sourse' = sourse $ inputFile configs
  let parseString' = parseString $ delemiter configs

  input <- sourse' $= parseString' $$ CL.consume

  let objects = convertFromCsv configs $ V.fromList input

  let startState = (-1.0 , M.empty)
  let result = evalState (clasifyState ((number configs), objects, rand, (percent configs))) startState

  runResourceT $ CL.sourceList (convertToCsv result) $$ CB.sinkIOHandle (buildOutputHandle configs)
