-- This implements a variant of the algorithm described in
-- "Asynchronous Anytime Sequential Monte Carlo". Note, I have not
-- proved this to be correct.
-- https://arxiv.org/abs/1407.2864

module Tyche.Inference.ParticleCascade where

import Tyche.Prob
import Tyche.Model
import Tyche.Random
import Tyche.Dist

import System.Random (split)
import Control.Monad.Trans.State.Lazy (get, put)

data Particle a = Particle
                  (ExecState a)
                  Double -- Weight, log space
                  [Int]  -- History

data ExecState a = InFlight (() -> Model a) -- Continuation
                 | Complete a               -- Return value

logWeight :: Particle a -> Double
logWeight (Particle _ lw _) = lw

isComplete :: Particle a -> Bool
isComplete (Particle (Complete _) _ _) = True
isComplete _ = False

pc :: Model a -> Rand [Particle a]
pc model = loop 0 (negInf) initialParticles
  where
    initialParticles = Particle (InFlight (const model)) 0 <$> map (:[]) [0..]
    loop :: Int -> Double -> [Particle a] -> Rand [Particle a]
    loop n wSum ps = do
      (g,g') <- split <$> get
      put g
      ps' <- mapM propagate ps
      put g'
      let (complete, rest) = span isComplete ps'
          n' = n + length complete
          wSum' = logSumExp $ wSum : map logWeight complete
        in do
        resampled <- resample n' wSum' rest
        rest' <- loop n' wSum' resampled
        return $ complete ++ rest'

pcD :: Ord a => Int -> Model a -> Rand (Dist a)
pcD n model = fromList <$> samples
  where samples = map toSample . take n <$> pc model

toSample :: Particle a -> (a, Double)
toSample (Particle (Complete x) lw _) = (x,lw)

propagate :: Particle a -> Rand (Particle a)
propagate (Particle (InFlight k) w h) = do
  (Particle e w' _) <- run $ k ()
  return $ Particle e (w+w') h
propagate p = return p

run :: Model a -> Rand (Particle a)
run (Pure x) = return $ Particle (Complete x) 0 []
run (Free (Belief dist k)) = do
  x <- sample dist
  run $ k x
run (Free (Weight lp k)) = return $ Particle (InFlight k) lp []

resample :: Int -> Double -> [Particle a] -> Rand [Particle a]
resample n wSum (p@(Particle e w h):ps) = do
  (g,g') <- split <$> get
  put g
  rest <- resample n' wSum' ps
  put g'
  (numOffspring, wout) <- branch n' wSum' p
  let offspring = map (Particle e wout . (:h)) (take numOffspring [n..])
  return $ offspring ++ rest
    where
      n' = n + 1
      wSum' = logAddExp wSum w

branch :: Int -> Double -> Particle a -> Rand (Int, Double)
branch n wSum (Particle (InFlight _) w _) = do
  numOffspring <- choose (r - fromIntegral rf) (return rc) (return rf)
  return (numOffspring, wAvg)
    where
      wAvg = wSum - log (fromIntegral n)
      r = exp (w - wAvg)
      rf = floor r
      rc = ceiling r
branch _ _ (Particle (Complete _) w _) = return (1,w)
