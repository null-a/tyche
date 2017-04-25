module Tyche.Random where

import System.Random (StdGen, mkStdGen, random)
import Control.Monad.Trans.State.Lazy (State, state, evalState)

type Rand a = State StdGen a

runRand :: Int -> Rand a -> a
runRand seed r = evalState r (mkStdGen seed)

-- Sample uniformly from [0,1)
rand :: Rand Double
rand = state random
