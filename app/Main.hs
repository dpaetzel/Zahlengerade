{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where


import Diagrams.Backend.SVG.CmdLine (mainWith)
import Data.Aeson -- for processing the input file (YAML)
import qualified Data.Yaml as Y -- for processing the input file (YAML)


import Text.Read (readMaybe)
import Data.Ratio (Ratio(..), (%))


import Zahlengerade


instance FromJSON NumberLine
instance ToJSON NumberLine


main :: IO ()
main = mainWith (fmap drawNumberLine . readInputFile)


{-|
Parses the given YAML file.
-}
readInputFile :: FilePath -> IO NumberLine
readInputFile path = do
  parsed <- Y.decodeFileEither path
  case parsed of
    Left err -> error . Y.prettyPrintParseException $ err
    Right nl -> return nl


interactiveDE :: IO NumberLine
interactiveDE = do
  start      <- ask "Von?" (Just $ -5 % 1)
  end        <- ask "Bis?" (Just $ 5 % 1)
  step       <- ask "Schrittgröße der nummerierten Markierungen?" (Just $ 1)
  mediumStep <- ask "Schrittgröße der mittleren Markierungen?" (Just $ 1 % 2)
  miniStep   <- ask "Schrittgröße der kleinsten Markierungen?" (Just $ 1 % 10)
  return $ Scaled
    { start       = start
    , end         = end
    , step        = step
    , mediumStep  = mediumStep
    , miniStep    = miniStep
    , annotations = []
    , size        = 1
    }


class Askable a where
  fromString :: String -> Maybe a
  ask'' :: String -> Maybe a -> IO a
  askDE'' :: String -> Maybe a -> IO a


instance (Integral a, Read a, Show a) => Askable (Ratio a) where
  fromString = readMaybe
  ask'' question def = ask (question ++ "(rational of the form \"a % b\")") def
  askDE'' question def = ask (question ++ "(Bruch in der Form \"a % b\")") def


instance Askable Double where
  fromString = readMaybe
  ask'' question def = ask (question ++ "(rational of the form \"a % b\")") def
  askDE'' question def = ask (question ++ "(Bruch in der Form \"a % b\")") def


instance Askable String where
  fromString = Just
  ask'' question def = ask (question ++ "(simple string)") def
  askDE'' question def = ask (question ++ "(einfache Zeichenkette)") def


instance Askable Int where
  fromString = readMaybe
  ask'' question def = ask (question ++ "(integer)") def
  askDE'' question def = ask (question ++ "(Ganzzahl)") def


{-|
Asks the user to supply a value optionally offering a default value.  If the
input given is invalid, ask her again.
-}
ask :: (Askable a, Show a) => String -> Maybe a -> IO a
ask question def = validateInput . ask' question $ fmap show def


validateInput :: Askable a => IO String -> IO a
validateInput action = do
  string <- action
  case fromString string of
    Just v   -> return v
    Nothing  -> do
      putStrLn "Invalid input!"
      validateInput action


ask' :: String -> Maybe String -> IO String
ask' question (Just def) = do
  putStr $ question ++ " "
  putStr $ "(default: " ++ def ++ ") "
  answer <- getLine
  if answer == ""
    then return def
    else return answer
ask' question Nothing = do
  putStr $ question ++ " "
  getLine
