{-# LANGUAGE DeriveGeneric #-}
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
import GHC.Generics -- for YAML parseability


import Diagrams.Prelude hiding (start, end)
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
data NumberLine = Free { annotations :: [(Rational, String)] }
  | Scaled
  { start       :: Rational
  , end         :: Rational
  , step        :: Rational
  , mediumStep  :: Rational
  , miniStep    :: Rational
  , annotations :: [(Rational, String)]
  }
  deriving (Generic) -- for YAML parseability

defaultScaled = Scaled
  { start = -5.0
  , end = 5.0
  , step = 1.0
  , mediumStep = 0.5
  , miniStep = 0.1
  , annotations = [(3.14, "pi")]
  }

instance Drawable NumberLine where
  draw size (Free annotations) =
    connect' (with & headLength .~ normal) "first" "last" scaleMarks -- # lw thick
    where
      scaleMarks :: Diagram B
      scaleMarks = strutX 1 # named "first" |||
                   position (map (\(x, y) -> (p2 (fromRational x, 0), draw 1 $ Annotation y)) annotations) |||
                   strutX 1 # named "last"
                   ===
                   strutY 1
  draw size (Scaled start end step mediumStep miniStep annotations) =
    connect' (with & headLength .~ normal) "first" "last" scaleMarks -- # lw thick
    where
      scaleMarks :: Diagram B
      scaleMarks = strutX 1 # named "first" |||
                   position (map (\(x, y) -> (p2 (fromRational x, 0), draw 1 y)) allMarks) |||
                   strutX 1 # named "last"
      allMarks :: [(Rational, ScaleMark)]
      allMarks = sortOn fst $ marks ++ mediumMarks ++ miniMarks ++ map (second Annotation) annotations
      marks = map (\x -> (x, StepMark . show $ fromRational x)) $ enumFromThenTo start (start + step) end
      mediumMarks = map (\x -> (x, MediumStepMark)) . filter (`notElem` map fst marks) $ enumFromThenTo start (start + mediumStep) end
      miniMarks = map (\x -> (x, MiniStepMark)) . filter (`notElem` map fst (marks ++ mediumMarks)) $ enumFromThenTo start (start + miniStep) end


data ScaleMark = MiniStepMark
               | MediumStepMark
               | StepMark String
               | Annotation String

instance Drawable ScaleMark where
  draw size mark = case mark of
    MiniStepMark     -> vrule miniStepStrokeSize
                        # lw thin
    MediumStepMark   -> vrule mediumStepStrokeSize
                        # lw thin
    StepMark label   -> (vrule stepStrokeSize
                        ===
                        drawLabel label)
                        # lw thin
                        alignY (1 - (stepStrokeSize / (stepStrokeSize + labelSize)))
    Annotation label -> (drawLabel label
                        ===
                        vrule annotationStrokeSize)
                        # lw thin
                        # alignB
    where
      labelSize = size / 2
      stepStrokeSize = size / 3
      mediumStepStrokeSize = miniStepStrokeSize * 2
      miniStepStrokeSize = stepStrokeSize / 4
      annotationStrokeSize = stepStrokeSize * 2
      drawLabel :: String -> Diagram B
      drawLabel l = square labelSize # opacity 0.0 <>
                    text l # fontSize (local (labelSize / 2))
