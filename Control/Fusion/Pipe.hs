{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, DeriveFunctor #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Fusion.Pipe where

import Control.Functor.Linear

-- | 1 & a, ie. the environment choses which sides happens.
-- type Perhaps a = forall k. Either k (a ⊸ k) ⊸ k 
type a & b = forall k. Either (a ⊸ k) (b ⊸ k) ⊸ k 
type Perhaps a = () & a

data PipeF a x where
  Done :: PipeF a x
  Ready :: Perhaps (a,x) ⊸ PipeF a x

instance LFunctor (PipeF a) where
  lfmap f = \case
    Ready k -> Ready (\case
                         Left k1 -> k (Left k1)
                         Right k1 -> k (Right (\(a,x) -> k1 (a,f x))))
    Done -> Done

instance LFunctor2 PipeF where
  lfmap2 f = \case
    Ready k -> Ready (\case
                         Left k1 -> k (Left k1)
                         Right k1 -> k (Right (\(a,x) -> k1 (f a, x))))
    Done -> Done

zipper :: (x ⊸ PipeF a x) -> (y ⊸ PipeF b y) -> (x,y) ⊸ PipeF (a,b) (x,y)
zipper φ ψ (s0,t0) = case (φ s0, ψ t0) of
  (Done, Done) -> Done
  (Done, Ready k) -> k (Left (\() -> Done))
  (Ready k, Done) -> k (Left (\() -> Done))
  (Ready k1, Ready k2) -> Ready (\case
                                    Left k -> k1 (Left (k2 (Left (\() -> k)))) -- inform both threads that we want to stop.
                                    Right k -> k1 (Right (\(a,s) -> k2 (Right (\(b,t) -> k ((a,b),(s,t)))))))
