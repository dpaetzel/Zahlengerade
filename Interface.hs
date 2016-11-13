-- module Interface where


-- simplest definition: all numbers at which a stroke shall be drawn must be
-- denoted in the input file!


import Text.Regex.PCRE


import System.Environment
import System.IO
-- import Control.Exception


import Diagrams.Backend.SVG.CmdLine (mainWith)


import Zahlengerade


-- TODO needs sorting?
{-|
Transforms a number line definition of absolute numbers with labels (e.g. as
given by the user) to a number line definition with relative distances between
the entries.
-}
steps :: [(Double, String)] -> NumberLine
steps [] = []
steps absolutes = steps' (fst . head $ absolutes) absolutes
  where
    steps' :: Double -> [(Double, String)] -> NumberLine
    steps' lastNum [] = []
    steps' lastNum ((num, label): rest) =
      (num - lastNum, labelFromString label) : steps' num rest

{-|
Tries to parse a line from the input file to the corresponding tuple.
-}
parseLine :: String -> Maybe (Double, String)
parseLine line = extractedTuple
  where
    -- TODO leave out the comma for “default label”: "^( *[0-9.]+ *)($|, *([^#]*)#?.*$)"
    pat = "^( *-? *[0-9.]+ *), *([^#]*)#?.*$"
    (_, _, _, matches) = line =~ pat :: (String, String, String, [String])
    extractedTuple :: Maybe (Double, String)
    extractedTuple = case matches of
      -- (number : (label : _)) -> Just (number, label)
      (number : (label : _)) -> fmap (flip (,) label) $ readDouble number
      _ -> Nothing
    readDouble string = case (reads string :: [(Double, String)]) of
      [(double, "")] -> Just double
      _ -> Nothing

{-|
Parses an actual input file (aborting if any line is malformed).
-}
readInputFile :: FilePath -> IO [(Double, String)]
readInputFile path = do
  contents <- readFile path
  let result = parseLines $ lines contents
  putStrLn . show $ lines contents
  return result

  where
    parseLines :: [String] -> [(Double, String)]
    parseLines (line: rest) =
      maybe (errorAt line) id (parseLine line) : parseLines rest
    parseLines [] = []
    errorAt line = error $ "Error when parsing line: " ++ line


main :: IO ()
main = do
  mainWith (fmap (drawNumberLine . steps) . readInputFile)
