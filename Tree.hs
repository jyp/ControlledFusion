{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Tree where

import qualified Fusion as F
import Data.Monoid
import Data.Foldable as T hiding (fold)

newtype MuTree a = Build { fold :: forall x . (x -> x -> x) -> (a -> x) -> x }

data Step a s = Done a | Yield s s

data NuTree a = forall s . Unfold s (s -> Step a s)

flattenMu :: MuTree a -> F.MuList a
flattenMu t = fold t (\ l r -> l `F.appendMu` r) return

revNuTree :: NuTree a -> NuTree a
revNuTree (Unfold s0 f) = Unfold s0 $ \ s -> case f s of
    Yield l r -> Yield r l
    Done a    -> Done a

{-# INLINE enumNuTree #-}
enumNuTree :: (Integral a,Num a,Ord a) => a -> a -> NuTree a
enumNuTree from to = Unfold (from,to) $ \ (lo,hi) ->
    if lo == hi
        then Done lo
        else
            let m = (hi - lo) `div` 2  + lo
            in  Yield (lo,m) (m+1,hi)

{-# INLINE sumNuTree #-}
sumNuTree :: Num a => NuTree a -> a
sumNuTree (Unfold s f) = go (f s)
  where
    go (Done x)    = x
    go (Yield l r) = go (f l) + go (f r)

test :: Int
test = sumNuTree (enumNuTree 0 1000)

{-# INLINE freeze #-}
freeze :: forall a. NuTree a -> MuTree a
freeze (Unfold s0 psi) = Build $ \ branch leaf ->
  let go s = case psi s of
          Done a -> leaf a
          Yield l r -> branch (go l) (go r)
  in  go s0

instance Foldable MuTree where
    -- foldMap :: Monoid m => (a -> m) -> MuTree a -> m
    {-# INLINE foldMap #-}
    foldMap f (Build g) = g (<>) f

{-# INLINE concatTree #-}
concatTree :: MuTree (MuTree a) -> MuTree a
concatTree (Build g) = Build $ \ branch leaf -> g branch (\ (Build t) -> t branch leaf)

instance Functor MuTree where
    {-# INLINE fmap #-}
    fmap f (Build g) = Build $ \ branch leaf -> g branch (leaf . f)

instance Monad MuTree where
    {-# INLINE return #-}
    return a = Build $ \ _ leaf -> leaf a
    {-# INLINE (>>=) #-}
    t >>= f = concatTree (fmap f t)

