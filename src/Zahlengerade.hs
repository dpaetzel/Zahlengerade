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


{-|
A number line can be either free (made up only from a set of numbers at each of
which a label is to be drawn) or scaled (containing a scale in addition to some
annotations).
-}
data NumberLine = Free [(Double, String)]
  | Scaled
  { start :: Double
  , end :: Double
  , step :: Double
  , miniStep :: Double
  , annotations :: [(Double, String)]
  }

instance Drawable NumberLine where
  draw size (Free annotations) = connect' (with & headLength .~ veryLarge) "first" "last" scaleMarks # lw veryThick
    where
      appendNext :: Diagram B -> (Double, (Double, String)) -> Diagram B
      appendNext acc (step, (value, label)) = acc ||| strutX step ||| draw 1 (Annotation label)
      scaleMarks :: Diagram B
      scaleMarks = foldl appendNext (strutX 1 # named "first") (steps fst annotations)
                   ||| strutX 3 # named "last"
  draw size scaled = draw size . Free . annotations $ scaled


{-|
Transforms a list to a list of relative distances between its entries by using
the supplied function to transform each of them into a numerical representation.
-}
steps :: Num a => (b -> a) -> [b] -> [(a, b)]
steps _ [] = []
steps f absolutes =
  let
    steps' _        []               = []
    steps' previous (current : rest) = (f current - previous, current) : steps' (f current) rest
  in
    steps' 0 absolutes


data ScaleMark = MiniStepMark
               | StepMark String
               | Annotation String

instance Drawable ScaleMark where
  draw size mark = case mark of
    MiniStepMark     -> vrule miniStepStrokeSize
    StepMark label   -> (vrule stepStrokeSize
                        -- ===
                        -- strutY (size / 4)
                        ===
                        drawLabel label)
                        # alignY (- (1 - (1 / (size + size))))
    Annotation label -> (drawLabel label
                        ===
                        vrule annotationStrokeSize
                        )
                        # alignY (- (1 - (1 / (annotationStrokeSize + size) * miniStepStrokeSize)))
                        -- # alignY (-0.875) -- for annotationStrokeSize = stepStrokeSize * 3 and
                                           -- miniStepStrokeSize = size / 2
                        # showOrigin
    where
      drawLabel :: String -> Diagram B
      drawLabel l = square size # opacity 0.0 <>
                    text l # fontSize (local size)
      stepStrokeSize = size
      miniStepStrokeSize = size / 2
      annotationStrokeSize = stepStrokeSize * 3
