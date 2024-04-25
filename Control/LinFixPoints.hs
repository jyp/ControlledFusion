{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, DeriveFunctor #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE EmptyCase #-}
module Control.FixPoints where

import Control.Functor.Linear

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
  Nu :: (x ⊸ f x) -> x ⊸ Nu f


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
-- given point in time. Also known as anamorphism.
nuAlloc' :: LFunctor f => Nu f ⊸ Fix f
nuAlloc' (Nu psi s0) = fixUnfold psi s0

-- | Reify a Mu into a Fix. Useful if many walks over the structure
-- are expected. Avoid if possible: this will allocate data into the
-- heap and won't fuse. However the structure will be built lazily;
-- thus it won't necessarily consume O(n) space at runtime at any
-- given point in time.
muAlloc :: Mu f ⊸ Fix f
muAlloc m = fold m In

-- | (Prepare) a bottom-up walk of the structure. Even if the language
-- were strict, the walk would happen only when folding the
-- result. Also known as catamorphism.
muWalk' :: LFunctor f => Fix f ⊸ Mu f
muWalk' s = Mu (\phi -> fixFold phi s)

-- | (Prepare) a top-down walk of the structure.  Even if the language
-- were strict, the walk would happen only when unfolding the result.
nuWalk :: Fix f ⊸ Nu f
nuWalk = Nu out

-- | Allocate an intermediate data structure. Even with a linear type,
-- this function does need to allocate some intermediate data, because
-- the input (Mu) controls the production, and (Nu) controls the
-- consumption. Hence, the output may 
alloc :: Mu f ⊸ Nu f
alloc = nuWalk ∘ muAlloc
-- alloc m = Nu out  (fold m In)


-- | 'muWalk . nuAlloc', fused. The intermediate Fix structure is not allocated as such,
-- but there is a loop in here, so this function will cause the data
-- structure to be reified at the time of the final fold
-- (unless the algebra is lazy). So expect O(depth) stack to be used.
loop :: LFunctor f => Nu f ⊸ Mu f
loop (Nu psi s0) = Mu (\ phi ->
     let -- this is a function from x (from phi) to y
         go = phi ∘ lfmap go ∘ psi
     in go s0)
-- go = fixFold phi . fixUnfold psi
--    = phi . fmap (fixFold phi) . out . In . fmap (fixUnfold psi) . psi
--    = phi . fmap (fixFold phi) . fmap (fixUnfold psi) . psi
--    = phi . fmap (fixFold phi . fixUnfold psi) . psi
--    = phi . fmap go . psi

-- We can now if we so desire give definitions of muWalk and nuAlloc in terms of
-- loop. These definitions will properly fuse (because they compose with a ).
-- This shows that we need a single loop combinator.

muWalk :: LFunctor f => Fix f ⊸ Mu f
muWalk = loop ∘ nuWalk

nuAlloc :: LFunctor f => Nu f ⊸ Fix f
nuAlloc = muAlloc ∘ loop

------------------
-- other functions

mapMu :: (forall x. f x ⊸ g x) -> Mu f ⊸ Mu g
mapMu f (Mu phi0) = Mu (\phi1 -> phi0 (phi1 ∘ f))

mapNu :: (forall x. f x ⊸ g x) -> Nu f ⊸ Nu g
mapNu f (Nu psi0 s) = Nu (f ∘ psi0) s


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
while' initial step1 step2 = fold (natLoop (Nu step1 initial)) step2

instance LFunctor (WhileF s) where
  lfmap f = \case
    Done s -> Done s
    Cont x -> Cont (f x)
data WhileF s x = Done s | Cont x

data Void

while :: s ⊸ (s ⊸ WhileF r s) -> (r ⊸ Void) -> Void
while initial step k = fold (loop (Nu step initial)) (\case
  Done r -> k r
  Cont x -> case x of)
