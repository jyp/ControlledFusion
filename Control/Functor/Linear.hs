{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, DeriveFunctor #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Control.Functor.Linear where

class LFunctor f where
  lfmap :: (a ⊸ b) -> f a ⊸ f b

class LFunctor2 f where
  lfmap2 :: (a ⊸ b) -> f a x ⊸ f b x
