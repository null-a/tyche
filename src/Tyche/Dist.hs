{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tyche.Dist where

import Tyche.Prob
import Tyche.Random
import Data.Function (on)
import Data.List (groupBy,sortBy)
import Data.Ord (comparing)

data Dist a where
  Bernoulli :: Prob -> Dist Bool
  Delta :: a -> Dist a
  -- Assumes that:
  -- Probabilities sum to one (which implies the list is non-empty)
  -- The as are distinct.
  Discrete :: [(a,Prob)] -> Dist a
  Uniform :: Dist Double

deriving instance Show a => Show (Dist a)

logProb :: Eq a => Dist a -> a -> LogProb

logProb (Bernoulli p) True = log p
logProb (Bernoulli p) False = log (1-p)

logProb (Delta x) y | x == y = 0
                    | otherwise = negInf

logProb (Discrete assoc) x = maybe negInf log (lookup x assoc)

logProb Uniform x | (0 <= x) && (x <= 1) = 0
                  | otherwise = negInf

toList :: Dist a -> [(a,LogProb)]
toList (Bernoulli p) = [(True,log p),(False,log (1-p))]
toList (Delta x) = [(x,0)]
toList (Discrete assoc) = (fmap . fmap) log assoc
toList _ = error "Not enumerable."

fromList :: Ord a => [(a,LogProb)] -> Dist a
fromList = Discrete . normalize . collect

collect :: Ord a => [(a,LogProb)] -> [(a,LogProb)]
collect = map combine . groupBy ((==) `on` fst) . sortBy (comparing fst)
  where
    combine pairs@((x,_):_) = (x, logSumExp (map snd pairs))
    combine [] = error "Impossible."

normalize :: [(a,LogProb)] -> [(a,Prob)]
normalize pairs = if logNorm > negInf
                  then (fmap . fmap) (exp . (subtract logNorm)) pairs
                  else error "Can't normalize."
  where logNorm = logSumExp (map snd pairs)

sample :: Dist a -> Rand a
sample (Bernoulli p) = (< p) <$> rand
sample (Delta x) = return x
sample (Discrete assoc) = loop assoc 1
   where
     loop [] _ = error "Something went wrong."
     loop [(x,_)] _ = return x
     loop ((x,p):rest) remaining = do
       r <- rand
       if r < p/remaining
       then return x
       else loop rest (remaining-p)
-- The end points are wrong here, one should be included.
sample Uniform = rand
