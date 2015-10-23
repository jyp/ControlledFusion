
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Control.FixPoints where

-- | Both Mu and Nu
newtype Fix f = In {out :: f (Fix f)}

-- | Negative view of Fix. A computation that can consume the structure, on demand.
newtype Mu f = Mu {fold :: forall x. (f x -> x) -> x}

-- | Positive view of Fix. Some data that can generate the structure, on demand.
data Nu f where
  Unfold :: (x -> f x) -> x -> Nu f

-- | Fold a Fix
fixFold :: Functor f => (f a -> a) -> Fix f -> a
fixFold f = f . fmap (fixFold f) . out

-- | Unfold a Fix
fixUnfold :: Functor f => (a -> f a) -> a -> Fix f
fixUnfold f = In . fmap (fixUnfold f) . f

-- | Reify a Nu into a Fix. Useful if many walks over the structure
-- are expected. Avoid if possible: this will allocate data into the
-- heap and won't fuse. However the structure will be built lazily;
-- thus it won't necessarily consume O(n) space at runtime at any
-- given point in time.
nuAlloc :: Functor f => Nu f -> Fix f
nuAlloc (Unfold psi s0) = fixUnfold psi s0

-- | Reify a Mu into a Fix. Useful if many walks over the structure
-- are expected. Avoid if possible: this will allocate data into the
-- heap and won't fuse. However the structure will be built lazily;
-- thus it won't necessarily consume O(n) space at runtime at any
-- given point in time.
muAlloc :: Mu f -> Fix f
muAlloc m = fold m In

-- | Prepare a bottom-up walk of the structure.
muWalk :: Functor f => Fix f -> Mu f
muWalk s = Mu $ \phi -> fixFold phi s

-- | Prepare a top-down walk of the structure
nuWalk :: Fix f -> Nu f
nuWalk = Unfold out

alloc :: Mu f -> Nu f
alloc = nuWalk . muAlloc

-- | 'muWalk . nuAlloc', fused. The intermediate Fix structure is not allocated as such,
-- but there is a loop in here, so this function will cause the data
-- structure to be reified on the stack at the time of the final fold
-- (unless the algebra is lazy). So expect O(depth) stack to be used.
loop :: Functor f => Nu f -> Mu f
loop (Unfold psi s0) = Mu $ \phi -> let go = phi . fmap go . psi
                                    in go s0
-- go = fixFold phi . fixUnfold psi
--    = phi . fmap (fixFold phi) . out . In . fmap (fixUnfold psi) . psi
--    = phi . fmap (fixFold phi) . fmap (fixUnfold psi) . psi
--    = phi . fmap (fixFold phi . fixUnfold psi) . psi
--    = phi . fmap go . psi
