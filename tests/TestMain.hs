module Main where

import Tyche.Model
import Tyche.Prelude
import Tyche.Random
import Tyche.Inference.Enumerate
import Tyche.Inference.LikelihoodWeighting

import Control.Monad (when)

model :: Model (Bool, Bool)
model = do
  x <- toss 0.4
  y <- toss 0.6
  when (x || y) $ weight (-1)
  return (x,y)

main :: IO ()
main = do
  print $ enumD model
  print $ runRand 0 $ lwD 100 model
