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

-- =====  input parse  =====

convertFromCsv :: V.Vector (Row String) -> [[Double]]
convertFromCsv = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow
          processRow = map (fromMaybe 0.0) . filter isJust . map maybeRead
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])

main :: IO ()
main = do
  let csvOpts = defCSVSettings {csvSep = ',', csvQuoteChar = Nothing}

  input <- runResourceT $ readCSVFile csvOpts "./examples/butterfly.txt"

  print $ convertFromCsv input
