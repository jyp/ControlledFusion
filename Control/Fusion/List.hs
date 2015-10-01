{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Control.Fusion.List (module Control.Fusion.List, module Reexport) where

import Control.Fusion.List.Mu as Reexport
import Control.Fusion.List.Nu as Reexport
-- import Control.Fusion.List.NonEmptyNu as Reexport hiding (Step(..),Unfold)

import Data.Foldable as F hiding (fold)
import Data.Maybe

foldMuNu :: (a -> b -> c -> c) -> c -> MuList a -> NuList b -> c
foldMuNu f z xs = foldMu xs (\h r ys -> case viewNu ys of
                                          Done -> z
                                          Yield y ys' -> f h y (r ys')) -- t is fed the smaller list.
                         (\_ -> z)

zipMuNu :: MuList a -> NuList b -> MuList (a,b)
zipMuNu xs ys = Build $ \cons nil -> foldMuNu (\x y t -> (x,y) `cons` t) nil xs ys

reverseMuNu :: MuList a -> NuList a
reverseMuNu xs = F.foldl (flip consNu) nilNu xs



-------------------------
-- Conversion functions


newtype Fix f = In {out :: f (Fix f)}
type FixList a = Fix (Step a)


-- | This reifies the list taken as input; there is no way that it'll be
-- eliminated.
{-# INLINE thaw #-}
thaw :: MuList a -> NuList a
thaw (Build g) = Unfold (g fixCons fixNil) out
  where fixNil = In Done
        fixCons a x = In (Yield a x)

-- | Perform a non-tail recursive, lazy, right-fold.
-- This loop cannot be fused with another loop. Attention: this
-- function will reify a list ON THE STACK if the "cons" that it uses is
-- strict.
{-# INLINE loopStack #-}
loopStack :: forall a. NuList a -> MuList a
loopStack (Unfold s0 psi) = Build $ \ cons nil ->
  let go_freeze s = case psi s of
          Done -> nil
          Yield a s' -> cons a (go_freeze s')
  in  go_freeze s0
