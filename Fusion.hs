{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

-- (c) JP Bernardy; GPL-licensed.

module Fusion where

import Data.List (unfoldr)
import Control.Comonad
import Control.Monad
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Monoid
import Data.Maybe

-- Principles:
-- Avoid using freeze
-- Try to produce Mu lists
-- Try to consume Nu lists
-- When writing transformations, try to preserve.

-------------
-- Mu lists

-- Fα = 1 + a × α
-- MuList a = μF
newtype MuList a = Build {fold :: forall x. (a -> x -> x) -> x -> x}

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

instance Monad MuList where
  return x = Build $ \cons nil -> cons x nil
  x >>= f = concatMu (fmap f x) -- efficient!

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
enumFromMu :: Int -> MuList Int
enumFromMu n = Build $ \cons nil -> cons n (fold (enumFromMu (n+1)) cons nil)
-- This is bad because we use gen. rec. here!
-}

headMu :: MuList a -> a
headMu (Build g) = g (\h _ -> h) (error "headMu: empty list")

---------------
--  Nu lists

-- NuList a = νF

data NuList a where
  Unfold :: s -> (s -> Step a s) -> NuList a

instance Show a => Show (NuList a) where
  show = show . nuToList

{-
concatMapNu :: (a -> NuList b) -> NuList a -> NuList b
concatMapNu f (Unfold sa0 nexta) = Unfold (sa0, Nothing) (uncurry next) where
  next sa Nothing = case nexta sa of
    Done -> Done
    Yield a sa' -> next sa' (Just (f a)) -- ! uses general recursion !
  next sa (Just (Unfold sb nextb)) = case nextb sb of
    Done -> next sa Nothing
    Yield b sb' -> Yield b (sa,Just (Unfold sb' nextb))

instance Monad NuList where
  return x = Unfold True $ \s -> case s of
    True -> Yield x False
    False -> Done
  (>>=) = flip concatMapNu -- Not *really* a monad: uses general recursion.
-}

data Step a s = Done | Yield a s

{-# INLINE stepToMaybe #-}
stepToMaybe :: Step t t1 -> Maybe (t, t1)
stepToMaybe Done = Nothing
stepToMaybe (Yield a s) = Just (a,s)

{-# INLINE maybeToStep #-}
maybeToStep :: Maybe (t, t1) -> Step t t1
maybeToStep Nothing = Done
maybeToStep (Just (a,s)) = Yield a s

{-# INLINE nuToList #-}
nuToList :: NuList a -> [a]
nuToList (Unfold s psi) = unfoldr (stepToMaybe . psi) s


{-# INLINE nuFromList #-}
nuFromList :: [a] -> NuList a
nuFromList = listToNu

{-# INLINE listToNu #-}
listToNu :: [a] -> NuList a
listToNu xs0 = Unfold xs0 go
  where go [] = Done
        go (x:xs) = Yield x xs

instance Functor NuList where
  fmap f (Unfold s g) = Unfold s $ \x -> case g x of
    Done -> Done
    Yield a t -> Yield (f a) t

{-# INLINE zipNu #-}
zipNu :: NuList a -> NuList b -> NuList (a,b)
zipNu = zipWithNu (,)

{-# INLINE zipWithNu #-}
zipWithNu :: (a -> b -> c) -> NuList a -> NuList b -> NuList c
zipWithNu f (Unfold s1 psi1) (Unfold s2 psi2) = Unfold (s1,s2) go
  where go (t1,t2) = case (psi1 t1,psi2 t2) of
           (Done,_) -> Done
           (_,Done) -> Done
           (Yield x u1,Yield y u2) -> Yield (f x y) (u1,u2)


{-# INLINE takeNu #-}
takeNu :: Int -> NuList a -> NuList a
takeNu n0 (Unfold s0 psi) = Unfold (n0,s0) (uncurry go) where
  go 0 _ = Done
  go n s = case psi s of
    Done -> Done
    Yield x t -> Yield x (n-1,t)

{-# INLINE takeWhileNu #-}
takeWhileNu :: (a -> Bool) -> NuList a -> NuList a
takeWhileNu p (Unfold s0 psi) = Unfold s0 go where
  go s = case psi s of
    Done -> Done
    Yield x t -> if p x then Yield x t else Done

{-# INLINE enumNu #-}
enumNu :: Int -> NuList Int
enumNu to = Unfold 0 $ \n -> if n < to then Yield n (n+1) else Done

{-# INLINE enumFromNu #-}
enumFromNu :: Int -> NuList Int
enumFromNu from = Unfold from $ \n -> Yield n (n+1)

{-# INLINE enumFromToNu #-}
enumFromToNu :: Int -> Int -> NuList Int
enumFromToNu from to = takeNu (to - from) (enumFromNu from)

{-# INLINE scanNu #-}
scanNu :: (b -> a -> b) -> b -> NuList a -> NuList b
scanNu f k (Unfold s0 psi) = Unfold (Just (k,s0)) go where
  go Nothing = Done
  go (Just (acc,s)) = case psi s of
    Done -> Yield acc Nothing
    Yield x t -> Yield acc (Just (f acc x,t))


{-# INLINE iterateNu #-}
iterateNu :: (a -> a) -> a -> NuList a
iterateNu f x0 = Unfold x0 go where
  go x = Yield x (f x)

{-
-- Do not fuse: use freeze
dropNu :: Int -> NuList a -> NuList a
dropNu n0 (Unfold s0 psi) = Unfold (go n0 s0) psi
  where go 0 s = s
        go n s = case psi s of
                      Yield _ t -> go (n-1) t
                      Done -> s

dropWhileNu :: (a -> Bool) -> NuList a -> NuList a
dropWhileNu p (Unfold s0 psi) = Unfold (go s0) psi
  where go s = case psi s of
                      Yield x t -> if p x then go t else t
                      Done -> s

splitAtNu :: Int -> NuList a -> (NuList a, NuList a)
splitAtNu n xs = (takeNu n xs,dropNu n xs)

spanNu :: (a -> Bool) -> NuList a -> (NuList a, NuList a)
spanNu p xs = (takeWhileNu p xs,dropWhileNu p xs)
-}


instance Comonad NuList where
  extract (Unfold s0 psi) = case psi s0 of
    Done -> error "extract: empty NuList"
    Yield x _ -> x
  duplicate (Unfold s0 psi) = Unfold s0 go where
    go s = case psi s of
      Done -> Done
      Yield _ t -> Yield (Unfold s psi) t

unfoldrNu :: (b -> Maybe (a, b)) -> b -> NuList a
unfoldrNu psi s = Unfold s (maybeToStep . psi)

repeatNu :: a -> NuList a
repeatNu a = Unfold () (const (Yield a ()))

cycleNu :: NuList a -> NuList a
cycleNu (Unfold s0 psi) = Unfold (s0,s0) psi'
  where psi' (s,s') =
          case psi s of
            Done -> case psi s' of
              Done -> Done -- exception empty list?
              Yield a s'' -> Yield a (s'',s')
            Yield a s'' -> Yield a (s'',s')

cycleNu' :: NuList a -> NuList a
cycleNu' (Unfold s0 psi) = case psi s0 of
  Done -> error "cycleNu': empty list"
  _ -> Unfold s0 psi' where
    psi' s = case psi s of
            Done -> psi s0
            y -> y

-- TODO: check if fusion happens in the above case.

-- intersperseNu see coutts & al


consNu' :: a -> NuList a -> NuList a
consNu' a0 (Unfold s0 psi) = Unfold Nothing psi'
  where psi' Nothing = Yield a0 (Just s0)
        psi' (Just s) = case psi s of
          Done -> Done
          Yield b s' -> Yield b (Just s')

consNu :: a -> NuList a -> NuList a
consNu a0 (Unfold s0 psi) = Unfold (Just (a0,s0)) psi'
  where psi' Nothing = Done
        psi' (Just (a,s)) = case psi s of
          Done -> Yield a Nothing
          Yield b s' -> Yield a (Just (b,s'))

nilNu :: NuList a
nilNu = Unfold () (const Done)

headNu :: NuList a -> a
headNu (Unfold s psi) = case psi s of
                          Done -> error "headNu: empty list"
                          Yield a _ -> a

tailNu :: NuList a -> NuList a
tailNu (Unfold s psi) = Unfold s' psi
  where s' = case psi s of
               Done -> error "tailNu: empty list"
               Yield _ s'' -> s''

-------------------------
-- Sinks

instance Foldable NuList where
  foldMap f = foldNu (\b a -> b `mappend` f a) mempty


{-# INLINE sumNu #-}
sumNu :: NuList Int -> Int
sumNu = foldNu (+) 0

-- 'foldl' (implemented with accumulator).  It's ok to use general
-- recursion here because it is the end of the pipeline.
{-# INLINE foldNu #-}
foldNu :: (b -> a -> b) -> b -> NuList a -> b
foldNu f k (Unfold s0 psi) = go k s0
  where go acc s = case psi s of
          Done -> acc
          Yield x t  -> go (f acc x) t

-- In reality though, this should be implemented as:
-- last . freeze . scanNu
foldNu' :: (b -> a -> b) -> b -> NuList a -> b
foldNu' f k xs = fromJust $ lastMu $ freeze $ scanNu f k xs


-------------------------
-- Conversion functions

-- No general rec: can be fused away.

newtype Fix f = In {out :: f (Fix f)}
type FixList a = Fix (Step a)

thaw :: MuList a -> NuList a
thaw (Build g) = Unfold (g fixCons fixNil) out
  where fixNil = In Done
        fixCons a x = In (Yield a x)

-- Uses general recursion: cannot be fused!
{-# INLINE freeze #-}
freeze :: forall a. NuList a -> MuList a
freeze (Unfold s0 psi) = Build $ \ cons nil ->
  let go s = case psi s of
          Done -> nil
          Yield a s' -> cons a (go s')
  in go s0

freezeList :: forall a. [a] -> MuList a
freezeList xs = Build $ \cons nil -> Prelude.foldr cons nil xs

