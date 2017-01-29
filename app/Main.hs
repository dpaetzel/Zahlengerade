module Main where


import Diagrams.Backend.SVG.CmdLine (mainWith)
-- for processing the input file (YAML)
import Data.Aeson
import qualified Data.Yaml as Y


import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import System.IO (readFile)


import Zahlengerade


instance FromJSON NumberLine
instance ToJSON NumberLine


main :: IO ()
-- main = mainWith (fmap (draw 2 . Free) . readInputFile)
-- main = mainWith (draw 1 Scaled
--                  { start = -3.0
--                  , end = 6.0
--                  , step = 1.0
--                  , miniStep = 0.1
--                  , mediumStep = 0.5
--                  , annotations = [(3.1415, "π")]
--                  })
-- main = print $ Y.encode defaultScaled
main = mainWith (fmap (draw 2) . readInputFile)


{-|
Parses an actual input file (aborting if any line is malformed).
-}
readInputFile :: FilePath -> IO NumberLine
readInputFile path = do
  parsed <- Y.decodeFileEither path
  case parsed of
    Left err -> error . Y.prettyPrintParseException $ err
    Right nl -> return nl
