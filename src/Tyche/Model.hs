{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Tyche.Model where

import Tyche.Prob
import Tyche.Dist

data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free x) = Free ((fmap . fmap) f x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  Pure f <*> x = fmap f x
  Free f <*> x = Free $ fmap (<*> x) f

instance Functor f => Monad (Free f) where
  return = Pure
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure x >>= f = f x
  Free x >>= f = Free $ fmap (>>= f) x

liftF :: Functor f => f r -> Free f r
liftF command = Free (fmap Pure command)

data ModelF next where
  Belief :: Dist a -> (a -> next) -> ModelF next
  Weight :: LogProb -> (() -> next) -> ModelF next

instance Functor ModelF where
  fmap :: (a -> b) -> ModelF a -> ModelF b
  fmap f (Belief dist g) = Belief dist (f . g)
  fmap f (Weight prob g) = Weight prob (f . g)

type Model = Free ModelF

belief :: Dist a -> Model a
belief dist = liftF $ Belief dist id

weight :: LogProb -> Model ()
weight lp = liftF $ Weight lp id
