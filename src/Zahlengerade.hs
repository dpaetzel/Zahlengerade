{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


{-|
Module      : Zahlengerade
Description : Very simple library for rendering number lines using the
              <http://projects.haskell.org/diagrams/ diagrams> library.
Copyright   : (c) David Pätzel
License     : GPL-3
Maintainer  : david.a.paetzel@gmail.com
Stability   : experimental
Portability : POSIX
-}


module Zahlengerade
  ( defaultScaled
  , drawNumberLine
  , NumberLine (Free, Scaled)
  )
where


import Control.Arrow (second)
import GHC.Generics -- for parseability using Aeson/YAML
import Text.Regex


import Diagrams.Prelude hiding (size, start, end)
import Diagrams.Backend.SVG


{-|
A number line can be either free (made up only from a set of numbers at each of
which a label is to be drawn) or scaled (containing a scale in addition to some
annotations).

Rationals must be used in order to properly test for list element-ness
('Double's are unusable because of their natural deviation).
-}
data NumberLine =
  Free
    { annotations :: [(Rational, String)]
    , size :: Double
    }
  | Scaled
    { start       :: Rational
    , end         :: Rational
    , step        :: Rational
    , mediumStep  :: Rational
    , miniStep    :: Rational
    , annotations :: [(Rational, String)]
    , size :: Double
    }
    deriving (Generic) -- for YAML parseability

defaultScaled :: NumberLine
defaultScaled = Scaled
  { start = -5.0
  , end = 5.0
  , step = 1.0
  , mediumStep = 0.5
  , miniStep = 0.1
  , annotations = [(3.14, "pi")]
  , size = 1
  }

drawNumberLine :: NumberLine -> Diagram B
drawNumberLine (Free annotations size) =
  drawArrow scaleMarks -- # lw thick
  ===
  strutY 1
  where
    scaleMarks :: Diagram B
    scaleMarks = drawMarks size . map (second Annotation) $ annotations
drawNumberLine (Scaled start end step mediumStep miniStep annotations size) =
  drawArrow scaleMarks -- # lw thick
  where
    scaleMarks :: Diagram B
    scaleMarks = drawMarks size allMarks
    allMarks :: [(Rational, ScaleMark)]
    allMarks = marks ++ mediumMarks ++ miniMarks ++ map (second Annotation) annotations
    marks :: [(Rational, ScaleMark)]
    marks = toMarks (StepMark . showGerman . fromRational) $ enumFromThenTo start (start + step) end
    mediumMarks :: [(Rational, ScaleMark)]
    mediumMarks = toMarks (const MediumStepMark) . fromThenToSkip (map fst marks) start (start + mediumStep) $ end
    miniMarks :: [(Rational, ScaleMark)]
    miniMarks = toMarks (const MiniStepMark) . fromThenToSkip (map fst (marks ++ mediumMarks)) start (start + miniStep) $ end


drawArrow :: Diagram B -> Diagram B
drawArrow marks =
  connect' (with & headLength .~ normal) "first" "last" $
  strutX 1 # named "first" ||| marks ||| strutX 1 # named "last"


drawMarks :: Double -> [(Rational, ScaleMark)] -> Diagram B
drawMarks size marks = position (map (\(x, y) -> (p2 (fromRational x, 0), drawMark size y)) marks)


toMarks :: (Rational -> ScaleMark) -> [Rational] -> [(Rational, ScaleMark)]
toMarks f = map (\x -> (x, f x))


fromThenToSkip :: [Rational] -> Rational -> Rational -> Rational -> [Rational]
fromThenToSkip skip from thenn = filter (`notElem` skip) . enumFromThenTo from thenn


data ScaleMark = MiniStepMark
               | MediumStepMark
               | StepMark String
               | Annotation String

drawMark size mark = case mark of
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
    labelSize = size / 4
    stepStrokeSize = labelSize / 2
    mediumStepStrokeSize = miniStepStrokeSize * 2
    miniStepStrokeSize = stepStrokeSize / 4
    annotationStrokeSize = stepStrokeSize * 2
    drawLabel :: String -> Diagram B
    drawLabel l = square labelSize # opacity 0.0 <>
                  text l # fontSize (local (labelSize / 2)) # font "arial"


{-|
Show a floating point number using German style (with a comma as a decimal mark
instead of a point).
-}
showGerman :: Double -> String
showGerman double = subRegex (mkRegex "\\.") (show double) ","
