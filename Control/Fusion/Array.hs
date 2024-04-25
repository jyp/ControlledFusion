{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, DeriveFunctor #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Control.Fusion.Array where

import Control.Functor.Linear


-- | A "pull" array of size cardinality of n. The recursive position x isn't used.
data Array n a x where
  Give :: (n -> a) -> Array n a x

instance LFunctor (Array n a) where
  lfmap _ (Give g) = Give g
instance LFunctor2 (Array n) where
  lfmap2 f (Give g) = Give (\i -> f (g i))
