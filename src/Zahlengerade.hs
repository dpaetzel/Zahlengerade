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
module Zahlengerade
  -- ( steps
  -- , drawNumberLine)
where


import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


class Drawable a where
  draw :: Double -> a -> Diagram B


data Label = RatLabel Rational | StrLabel String

{-|
Labels are drawable; in order for the label's text to “take up space”, it gets
surrounded by a transparent square (actually, it is 'atop' of a transparent
square).
-}
instance Drawable Label where
  draw size (RatLabel _) = draw size $ StrLabel "n.n."
  draw size (StrLabel l) = square size # opacity 0.0 <>
                           text l # fontSize (local size)

-- TODO proper implementation using Read?
labelFromString :: String -> Label
labelFromString = StrLabel


{-|
A mark on a number line consisting of a little stroke and a label below it.
-}
data ScaleMark = ScaleMark Label
instance Drawable ScaleMark where
  draw size (ScaleMark label) = stroke
                                ===
                                strutY (size / 4)
                                ===
                                draw size label
    where
      stroke :: Diagram B
      stroke = vrule (size / 2) -- # lw veryThick


{-|
A number line is made up from a set of steps after each of which a 'scaleMark'
is to be drawn (containing corresponding label).
-}
newtype NumberLine = NumberLine [(Step, Label)]
type Step = Double

instance Drawable NumberLine where
  draw size (NumberLine dat) = connect' (with & headLength .~ veryLarge) "first" "last" scaleMarks # lw veryThick
    where
      appendNext acc (step, label) = acc ||| strutX step ||| draw 1 (ScaleMark label)
      scaleMarks :: Diagram B
      scaleMarks = foldl appendNext (strutX 1 # named "first") dat
                   ||| strutX 3 # named "last"


{-|
Transforms a list of absolute numbers with labels (e.g. as given by the
user) to a list of relative distances between the entries (not sorting them
beforehand).
-}
steps :: [(Double, String)] -> [(Step, Label)]
steps [] = []
steps absolutes = steps' (fst . head $ absolutes) absolutes
  where
    steps' :: Double -> [(Double, String)] ->  [(Step, Label)]
    steps' _ [] = []
    steps' lastNum ((num, label): rest) =
      (num - lastNum, labelFromString label) : steps' num rest


{-|
Creates a diagram from a 'NumberLine'.
-}
-- drawNumberLine :: NumberLine -> Diagram B
-- drawNumberLine nl = connect' (with & headLength .~ veryLarge) "first" "last" scaleMarks # lw veryThick
--   where
--     length = sum . map fst $ nl

--     appendNext acc (step, label) = acc ||| strutX step ||| draw 1 (ScaleMark label)
--     scaleMarks :: Diagram B
--     scaleMarks = foldl appendNext (strutX 1 # named "first") nl
--                  ||| strutX 3 # named "last"

-- TODO remove drawnumberline, fix errors

-- testLine :: NumberLine
-- testLine = map (\n -> (1, IntegerLabel n)) numbers
--   where
--     numbers = [0..10]
