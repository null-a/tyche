module Tyche.Prelude where

import Tyche.Dist
import Tyche.Model
import Tyche.Prob

import Control.Monad (unless, replicateM)

condition :: Bool -> Model ()
condition p = unless p (weight negInf)

observe :: Eq a => Dist a -> a -> Model ()
observe dist val = weight $ logProb dist val

-- aka flip in Church/WebPPL
toss :: Prob -> Model Bool
toss = belief . Bernoulli

binomial :: Int -> Prob -> Model Int
binomial n p = length . filter id <$> replicateM n (toss p)
