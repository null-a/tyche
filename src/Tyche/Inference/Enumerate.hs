module Tyche.Inference.Enumerate where

import Tyche.Prob
import Tyche.Dist
import Tyche.Model

enum :: Model a -> [(a,LogProb)]
enum (Pure x) = [(x,0)]
enum (Free (Belief dist k)) = do
  (x,lp) <- toList dist
  (y,lp2) <- enum (k x)
  return (y,lp+lp2)
enum (Free (Weight lp k)) = do
  (x,lp2) <- enum (k ())
  return (x,lp+lp2)

enumD :: Ord a => Model a -> Dist a
enumD = fromList . enum
