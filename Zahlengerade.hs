{-# LANGUAGE NoMonomorphismRestriction #-}


{-|
Module      : Zahlengerade
Description : Simple library for rendering number lines using the
              <http://projects.haskell.org/diagrams/ diagrams> library.
Copyright   : (c) David Pätzel, 2016
License     : GPL-3
Maintainer  : david.a.paetzel@gmail.com
Stability   : experimental
Portability : POSIX
-}
-- TODO add more general documentation above
module Zahlengerade where


import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


import Data.Ratio


data Label =
  DoubleLabel Double
  | IntegerLabel Integer
  | RationalLabel Rational
  | StringLabel String

instance Show Label where
  show (DoubleLabel d) = show d
  show (IntegerLabel i) = show i
  show (RationalLabel r) = show r
  show (StringLabel s) = s

-- TODO proper implementation
labelFromString :: String -> Label
labelFromString = StringLabel

{-|
Creates a diagram from the supplied label using the given size.  In order for
the label's text to “take up space”, it needs to be surrounded by e.g. a
transparent square (actually, be 'atop' of a transparent square).
-}
drawLabel :: Double -> Label -> Diagram B
drawLabel size label = square size # opacity 0.0 <>
                       labelText # fontSize (local size)
  where
    labelText :: Diagram B
    labelText = text (show label)

{-|
Creates a mark consisting of a little stroke and a label below it.
-}
scaleMark :: Double -> Label -> Diagram B
scaleMark size label = (stroke
                        ===
                        drawLabel size label)
  where
    stroke :: Diagram B
    stroke = vrule size


{-|
A number line is made up from a set of steps after each of which a 'scaleMark'
is to be drawn (containing corresponding label).
-}
type NumberLine = [(Step, Label)]
type Step = Double

{-|
Creates a diagram from a 'NumberLine'.
-}
drawNumberLine :: NumberLine -> Diagram B
drawNumberLine nl = connect "first" "last" scaleMarks
  where
    length = sum . map fst $ nl

    appendNext acc (step, label) = acc ||| strutX step ||| scaleMark 1 label
    scaleMarks :: Diagram B
    scaleMarks = foldl appendNext (strutX 1 # named "first") nl
                 ||| strutX 3 # named "last"


testLine :: NumberLine
testLine = map (\n -> (1, IntegerLabel n)) numbers
  where
    numbers = [0..10]

-- main :: IO ()
-- main = mainWith . drawNumberLine $ testLine
