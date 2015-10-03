{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

-- (c) JP Bernardy; GPL-licensed.

module Control.Fusion.List.Mu where

import Control.Comonad
import Control.Monad
import Data.Foldable as F hiding (fold)
import Data.Traversable
import Control.Applicative
import Data.Monoid

-- Principles:
-- Avoid using freeze
-- Try to produce Mu lists
-- Try to consume Nu lists
-- When writing transformations, try to preserve.

-------------
-- Mu lists

-- Fα = 1 + a × α
-- MuList a = μF

-- Attention: running foldMu will effectively perform a non-tail
-- recursive, lazy, right-fold.  This may therefore reify the list ON
-- THE STACK if the "cons" that it uses is strict.
newtype MuList a = Build {foldMu :: forall x. (a -> x -> x) -> x -> x}
    {-   {fold :: forall x. (forall k . a -> x -> (x -> k) -> k)
                         -> (forall k . (x -> k) -> k)
                         ->
    -}


instance Show a => Show (MuList a) where
  show = show . muToList

instance Functor MuList where
  fmap f (Build g) = Build $ \cons nil -> g (\h t -> cons (f h) t) nil

instance Foldable MuList where
  foldr c n (Build g) = g c n

instance Traversable MuList where  -- ????
   traverse f (Build g) = g (\h t -> consMu <$> f h <*> t) (pure nilMu)

instance Monoid (MuList a) where
  mempty = nilMu
  mappend = appendMu

{-# INLINE consMu #-}
consMu :: a -> MuList a -> MuList a
consMu x (Build g) = Build $ \cons nil -> cons x (g cons nil)

{-# INLINE snocMu #-}
snocMu :: a -> MuList a -> MuList a
snocMu x (Build g) = Build $ \cons nil -> g cons (cons x nil)

{-# INLINE appendMu #-}
appendMu :: MuList a -> MuList a -> MuList a
appendMu (Build f) (Build g) = Build $ \cons nil -> f cons (g cons nil)

{-# INLINE nilMu #-}
nilMu :: MuList a
nilMu = Build $ \_cons nil -> nil

{-# INLINE filterMu #-}
filterMu :: (a -> Bool) -> MuList a -> MuList a
filterMu p (Build g) = Build $ \cons nil -> g (\h t -> if p h then cons h t else t) nil

{-# INLINE concatMu #-}
concatMu :: MuList (MuList a) -> MuList a
concatMu (Build g) = Build $ \cons nil -> g (\(Build h) t -> h cons t) nil

instance Applicative MuList where
  pure = return
  (<*>) = ap
  
instance Monad MuList where
  return x = Build $ \cons nil -> cons x nil
  x >>= f = concatMu (fmap f x) -- efficient!

instance Alternative MuList where
  empty = nilMu
  (<|>) = appendMu

instance MonadPlus MuList where
  mzero = nilMu
  mplus = appendMu

{-# INLINE takeWhileMu #-}
takeWhileMu :: (a -> Bool) -> MuList a -> MuList a
takeWhileMu p (Build g) = Build $ \cons nil -> g (\h t -> if p h then cons h t else nil) nil

{-# INLINE takeMu #-}
takeMu :: Int -> MuList a -> MuList a
takeMu n (Build g) = Build $ \cons nil -> g (takeFB cons nil) (const nil) n

{-# INLINE takeFB #-}
takeFB :: (a -> b -> b) -> b -> a -> (Int -> b) -> Int -> b
takeFB c n x xs m | m <= 1  = x `c` n
                  | otherwise = x `c` xs (m - 1)

atMu :: MuList a -> Int -> a
atMu (Build g) n = g (\h t x -> if x == 0 then h else t (x-1)) (error "atMu: index not found") n

lastMu :: MuList a -> Maybe a
lastMu (Build g) = g (\h t -> case t of Nothing -> Just h; Just h' -> Just h') Nothing

{-# INLINE repeatMu #-}
repeatMu :: a -> MuList a
repeatMu x = Build $ \cons _nil -> let loop = cons x loop in loop

{-# INLINE replicateMu #-}
replicateMu :: Int -> a -> MuList a
replicateMu n x = takeMu n (repeatMu x)

-- Not sure how this behaves
cycleMu :: MuList a -> MuList a
cycleMu (Build g) = Build $ \cons _nil -> let loop = g cons loop in loop

muToList :: MuList a -> [a]
muToList (Build g) = g (:) []

{-

Cannot write this: it uses recursion (and we do not want control structures)

enumFromMu :: Int -> MuList Int
enumFromMu n = Build $ \cons nil -> cons n (fold (enumFromMu (n+1)) cons nil)

enumMu :: (Enum a, Ord a) => a -> a -> MuList a
enumMu from to | from > to = nilMu
               | otherwise = consMu from (enumMu (succ from) to)

-- | blerg: loops not allowed.
enumMu :: (Num a, Ord a) => a -> a -> MuList a
enumMu from to = Build $ \ cons nil ->
    let go n | n > to = nil
             | otherwise = cons n (go (n + 1))
    in  go from
-}

headMu :: MuList a -> a
headMu (Build g) = g (\h _ -> h) (error "headMu: empty list")

freezeList :: forall a. [a] -> MuList a
freezeList xs = Build $ \cons nil -> Prelude.foldr cons nil xs

