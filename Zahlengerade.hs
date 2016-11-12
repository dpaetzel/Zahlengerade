{-# LANGUAGE NoMonomorphismRestriction #-}


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
  show (StringLabel s) = show s

-- text has to be put into an (opaque) square in order to “take up space”
drawLabel :: Double -> Label -> Diagram B
drawLabel size label = square size # opacity 0.0 <> labelText # fontSize (local size)
  where
    labelText :: Diagram B
    labelText = text (show label)

-- a mark on a scale (consisting of a little stroke and a label below it)
scaleMark :: Double -> Label -> Diagram B
scaleMark size label = (stroke
                        ===
                        drawLabel size label)
  where
    stroke :: Diagram B
    stroke = vrule size


type Step = Double
type NumberLine = [(Step, Label)]

numberLine :: NumberLine -> Diagram B
numberLine nl = connect "first" "last" scaleMarks
  where
    length = sum . map fst $ nl

    appendNext acc (step, label) = acc ||| strutX step ||| scaleMark 1 label
    scaleMarks :: Diagram B
    scaleMarks = foldl appendNext (strutX 1 # named "first") nl ||| strutX 3 # named "last"


testLine :: NumberLine
testLine = map (\n -> (1, IntegerLabel n)) numbers
  where
    numbers = [0..10]

main :: IO ()
main = mainWith . numberLine $ testLine
