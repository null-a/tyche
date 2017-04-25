module Tyche.Prob where

import Data.List (foldl')

negInf :: Double
negInf = -1/0

-- By convention, in Prob space everything is expected to be
-- normalised.
-- It's assumed that values lie in [0,1].
type Prob = Double

-- Log probabilities are not necessarily normalised.
type LogProb = Double


logSumExp :: [Double] -> Double
logSumExp [] = negInf
logSumExp xs = if isInfinite m then m else log s + m
  where
    m = maximum xs
    s = foldl' (\acc x -> acc + exp (x - m)) 0 xs

logAddExp :: Double -> Double -> Double
logAddExp x y = log $ exp x + exp y
