module Main where


import Diagrams.Backend.SVG.CmdLine (mainWith)
import Text.Regex.PCRE


import System.IO (readFile)


import Zahlengerade


main :: IO ()
main = do
  mainWith (fmap (drawNumberLine . steps) . readInputFile)


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
