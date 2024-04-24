{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, DeriveFunctor #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Control.FixPoints where


-- | Both Mu and Nu
newtype Fix f = In (f (Fix f))
out :: Fix f ⊸ f (Fix f)
out (In x) = x

-- | Negative view of Fix. A computation that can consume the structure, on demand.
newtype Mu f = Mu (forall x. (f x ⊸ x) -> x)
fold :: Mu f ⊸ (f x ⊸ x) -> x
fold (Mu f) = f

-- | Positive view of Fix. Some data that can generate the structure, on demand.
data Nu f where
  Unfold :: (x ⊸ f x) -> x ⊸ Nu f

class LFunctor f where
  lfmap :: (a ⊸ b) -> f a ⊸ f b

class LFunctor2 f where
  lfmap2 :: (a ⊸ b) -> f a x ⊸ f b x

(∘) :: (b ⊸ c) ⊸ (a ⊸ b) ⊸ a ⊸ c
(f ∘ g) x = f (g x)

-- | Fold a Fix
fixFold :: LFunctor f => (f a ⊸ a) -> Fix f ⊸ a
fixFold f = f ∘ lfmap (fixFold f) ∘ out

-- | Unfold a Fix
fixUnfold :: LFunctor f => (a ⊸ f a) -> a ⊸ Fix f
fixUnfold f = In ∘ lfmap (fixUnfold f) ∘ f

-- | Reify a Nu into a Fix. Useful if many walks over the structure
-- are expected. Avoid if possible: this will allocate data into the
-- heap and won't fuse. However the structure will be built lazily;
-- thus it won't necessarily consume O(n) space at runtime at any
-- given point in time.
nuAlloc :: LFunctor f => Nu f ⊸ Fix f
nuAlloc (Unfold psi s0) = fixUnfold psi s0

-- | Reify a Mu into a Fix. Useful if many walks over the structure
-- are expected. Avoid if possible: this will allocate data into the
-- heap and won't fuse. However the structure will be built lazily;
-- thus it won't necessarily consume O(n) space at runtime at any
-- given point in time.
muAlloc :: Mu f ⊸ Fix f
muAlloc m = fold m In

-- | (Prepare) a bottom-up walk of the structure.
muWalk :: LFunctor f => Fix f ⊸ Mu f
muWalk s = Mu (\phi -> fixFold phi s)

-- | (Prepare) a top-down walk of the structure
nuWalk :: Fix f ⊸ Nu f
nuWalk = Unfold out

alloc :: Mu f ⊸ Nu f
alloc = nuWalk ∘ muAlloc

-- | 'muWalk . nuAlloc', fused. The intermediate Fix structure is not allocated as such,
-- but there is a loop in here, so this function will cause the data
-- structure to be reified on the stack at the time of the final fold
-- (unless the algebra is lazy). So expect O(depth) stack to be used.
loop :: LFunctor f => Nu f ⊸ Mu f
loop (Unfold @x psi s0) = Mu (\ phi ->
     let -- this is a function from x (from phi) to y
         go =phi ∘ lfmap go ∘ psi
     in go s0)
-- go = fixFold phi . fixUnfold psi
--    = phi . fmap (fixFold phi) . out . In . fmap (fixUnfold psi) . psi
--    = phi . fmap (fixFold phi) . fmap (fixUnfold psi) . psi
--    = phi . fmap (fixFold phi . fixUnfold psi) . psi
--    = phi . fmap go . psi

-- We can now if we so desire give definitions of muWalk and nuAlloc in terms of
-- loop. These definitions will properly fuse. This shows that we need a single
-- loop combinator.

muWalk' :: LFunctor f => Fix f ⊸ Mu f
muWalk' = loop ∘ nuWalk

nuAlloc' :: LFunctor f => Nu f ⊸ Fix f
nuAlloc' = muAlloc ∘ loop

------------------
-- other functions

mapMu :: LFunctor2 f => (a ⊸ b) -> Mu (f a) ⊸ Mu (f b)
mapMu f (Mu phi0) = Mu (\phi1 -> phi0 (phi1 ∘ lfmap2 f))


-- Examples:
data ListF a x = Stop | More a x deriving Functor


data NatF x = Z | S x

instance LFunctor NatF where
  lfmap f = \case
    Z -> Z
    S x -> S (f x)

natLoop :: Nu NatF ⊸ Mu NatF
natLoop = loop

-- unfortunately this is consuming stack space :(
while' :: s ⊸ (s ⊸ NatF s) -> (NatF r ⊸ r) -> r
while' initial step1 step2 = fold (natLoop (Unfold step1 initial)) step2

instance LFunctor (WhileF s) where
  lfmap f = \case
    Done s -> Done s
    Cont x -> Cont (f x)
data WhileF s x = Done s | Cont x

data Void

while :: s ⊸ (s ⊸ WhileF r s) -> (r ⊸ Void) -> Void
while initial step k = fold (loop (Unfold step initial)) (\case
  Done r -> k r
  Cont x -> case x of)
