
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Control.FixPoints where

newtype Mu f = Mu {fold :: forall x. (f x -> x) -> x}

data Nu f where
  Unfold :: (x -> f x) -> x -> Nu f

newtype Fix f = In {out :: f (Fix f)}

fixFold :: Functor f => (f a -> a) -> Fix f -> a
fixFold f = f . fmap (fixFold f) . out

fixUnfold :: Functor f => (a -> f a) -> a -> Fix f
fixUnfold f = In . fmap (fixUnfold f) . f

-- | Reifies the structure taken as input. However, the structure will be built lazily;
-- thus it won't necessarily consume O(n) space at runtime.
thaw :: Mu f -> Nu f
thaw (Mu fold) = Unfold out (fold In)

-- | Unfold then fold, fused. Still, there is a loop here, so this
-- will typically cause the data structure to be reified on the stack
-- at the time of the final fold (unless the algebra is lazy). Hence
-- the name: freeze.
freeze :: Functor f => Nu f -> Mu f
freeze (Unfold psi s0) = Mu $ \phi -> let go = phi . fmap go . psi
                                      in go s0
-- go = fixFold phi . fixUnfold psi
--    = phi . fmap (fixFold phi) . out . In . fmap (fixUnfold psi) . psi
--    = phi . fmap (fixFold phi) . fmap (fixUnfold psi) . psi
--    = phi . fmap (fixFold phi . fixUnfold psi) . psi
--    = phi . fmap go . psi
