{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Control.Fusion.Seq where

import Data.Foldable as F
import Data.Monoid

data Step a s = Done a | Yield s s
instance Functor (Step a) where
    {-# INLINE fmap #-}
    fmap _f (Done x) = Done x
    fmap f (Yield l r) = Yield (f l) (f r)

data Pull a = forall s . Unfold s (s -> Step a s)

instance Functor Pull where
    {-# INLINE fmap #-}
    fmap f (Unfold s0 g) = Unfold s0 $ \s -> case g s of Done x -> Done (f x); Yield l r -> Yield l r



revPull :: Pull a -> Pull a
revPull (Unfold s0 f) = Unfold s0 $ \ s -> case f s of
    Yield l r -> Yield r l
    Done a    -> Done a

{-# INLINE enumPull #-}
enumPull :: (Integral a,Num a,Ord a) => a -> a -> Pull a
enumPull from to = Unfold (from,to) $ \ (lo,hi) ->
    if lo == hi
        then Done lo
        else
            let m = (hi - lo) `div` 2  + lo
            in  Yield (lo,m) (m+1,hi)

-- Match a producer and a consumer into a single loop. 
-- wrt. to fusion this code acts as a 'sink' for both sides.
{-# INLINE thaw #-}
thaw :: forall a. Pull a -> Push a
thaw (Unfold s0 psi) = Build $ \ branch leaf ->
  let go s = case psi s of
          Done a -> leaf a
          Yield l r -> branch (go l) (go r)
  in  go s0

newtype Fix f = In {out :: f (Fix f)}

-- Allocate the structure in memory using the above fixpoint.
-- wrt. to fusion this acts as a 'source' for both sides.
{-# INLINE freeze #-}
freeze :: Push a -> Pull a
freeze (Build g) = Unfold (g fixApp fixLeaf) out
  where fixLeaf x = In (Done x)
        fixApp x y = In (Yield x y)

newtype Push a = Build { fold :: forall x. (x -> x -> x) -> (a -> x) -> x }

instance Foldable Push where
    -- foldMap :: Monoid m => (a -> m) -> Push a -> m
    {-# INLINE foldMap #-}
    foldMap f (Build g) = g (<>) f

{-# INLINE concatTree #-}
concatTree :: Push (Push a) -> Push a
concatTree (Build g) = Build $ \ branch leaf -> g branch (\ (Build t) -> t branch leaf)

instance Functor Push where
    {-# INLINE fmap #-}
    fmap f (Build g) = Build $ \ branch leaf -> g branch (leaf . f)

instance Monad Push where
    {-# INLINE return #-}
    return a = Build $ \ _ leaf -> leaf a
    {-# INLINE (>>=) #-}
    t >>= f = concatTree (fmap f t)







