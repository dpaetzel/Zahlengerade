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


import Data.List (sortOn)
import Control.Arrow (first, second)


import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


class Drawable a where
  draw :: Double -> a -> Diagram B


{-|
A number line can be either free (made up only from a set of numbers at each of
which a label is to be drawn) or scaled (containing a scale in addition to some
annotations).

Rationals must be used in order to properly test for list element-ness
('Double's are unusable because of their natural deviation).
-}
data NumberLine = Free [(Rational, String)]
  | Scaled
  { start :: Rational
  , end :: Rational
  , step :: Rational
  , mediumStep
  , miniStep :: Rational
  , annotations :: [(Rational, String)]
  }

instance Drawable NumberLine where
  draw size (Free annotations) =
    connect' (with & headLength .~ veryLarge) "first" "last" scaleMarks # lw veryThick
    where
      scaleMarks :: Diagram B
      scaleMarks = strutX 1 # named "first" |||
                   position (map (\(x, y) -> (p2 (fromRational x, 0), draw 1 $ Annotation y)) annotations) |||
                   strutX 3 # named "last"
  draw size (Scaled start end step mediumStep miniStep annotations) =
    connect' (with & headLength .~ veryLarge) "first" "last" scaleMarks # lw thick
    where
      scaleMarks :: Diagram B
      scaleMarks = strutX 1 # named "first" |||
                   position (map (\(x, y) -> (p2 (fromRational x, 0), draw 1 y)) allMarks) |||
                   strutX 3 # named "last"
      allMarks :: [(Rational, ScaleMark)]
      allMarks = sortOn fst $ marks ++ mediumMarks ++ miniMarks ++ map (second Annotation) annotations
      marks = map (\x -> (x, StepMark . show $ fromRational x)) $ enumFromThenTo start (start + step) end
      mediumMarks = map (\x -> (x, MediumStepMark)) . filter (`notElem` map fst marks) $ enumFromThenTo start (start + mediumStep) end
      miniMarks = map (\x -> (x, MiniStepMark)) . filter (`notElem` map fst (marks ++ mediumMarks)) $ enumFromThenTo start (start + miniStep) end


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
               | MediumStepMark
               | StepMark String
               | Annotation String

instance Drawable ScaleMark where
  draw size mark = case mark of
    MiniStepMark     -> vrule miniStepStrokeSize
                        # lw thin
                        -- # showEnvelope
    MediumStepMark   -> vrule mediumStepStrokeSize
                        # lw thin
    StepMark label   -> (vrule stepStrokeSize
                        -- ===
                        -- strutY (size / 4)
                        ===
                        drawLabel label)
                        # lw thin
                        # alignY (1 - (1 / (size + size)))
                        -- # showEnvelope
    Annotation label -> (drawLabel label
                        ===
                        vrule annotationStrokeSize
                        )
                        # lw thin
                        # alignY (- (1 - (1 / (annotationStrokeSize + size) * miniStepStrokeSize)))
                        -- # alignY (-0.875) -- for annotationStrokeSize = stepStrokeSize * 3 and
                                           -- miniStepStrokeSize = size / 2
                        -- # showOrigin
                        -- # showEnvelope
    where
      drawLabel :: String -> Diagram B
      drawLabel l = square size # opacity 0.0 <>
                    text l # fontSize (local (size / 2))
      stepStrokeSize = size
      miniStepStrokeSize = size / 4
      mediumStepStrokeSize = size / 2
      annotationStrokeSize = stepStrokeSize * 3
