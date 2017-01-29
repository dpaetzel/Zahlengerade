module Main where


import Diagrams.Backend.SVG.CmdLine (mainWith)
import Data.Aeson -- for processing the input file (YAML)
import qualified Data.Yaml as Y -- for processing the input file (YAML)


import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import System.IO (readFile)


import Zahlengerade


instance FromJSON NumberLine
instance ToJSON NumberLine


main :: IO ()
main = mainWith (fmap (draw 2) . readInputFile)


{-|
Parses the given YAML file.
-}
readInputFile :: FilePath -> IO NumberLine
readInputFile path = do
  parsed <- Y.decodeFileEither path
  case parsed of
    Left err -> error . Y.prettyPrintParseException $ err
    Right nl -> return nl
