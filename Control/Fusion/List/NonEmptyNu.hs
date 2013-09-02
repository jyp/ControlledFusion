{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Control.Fusion.List.NonEmptyNu where

import Control.Comonad
import Data.Foldable
import Data.Monoid hiding (Last)

---------------
--  Nu lists

-- NeNuList a = νF
data Step a s = Last a | Yield a s

data NeNuList a where
  Unfold :: s -> (s -> Step a s) -> NeNuList a

instance Show a => Show (NeNuList a) where
  show = show . toList

data Concat a s
    = Outer s
    | Inner (NeNuList a) (Maybe s)

{-
{-# INLINE concatNeNu #-}
concatNeNu :: NeNuList (NeNuList a) -> NeNuList a
concatNeNu (Unfold s0 f) = Unfold (Outer s0) $ \ s ->
    case s of
        Outer s' -> case f s' of
            Last (Unfold s1 g) -> case g s1 of
                Last x     -> Last x
                Yield x s2 -> Yield x (Inner (Unfold s2 g) Nothing)
            Yield (Unfold s1 g) so -> case g s1 of
                Last x     -> Yield x (Outer so)
                Yield x s2 -> Yield x (Inner (Unfold s2 g) (Just so))
        Inner (Unfold s1 g) mso -> case g s1 of
            Last x     -> case mso of
                Nothing -> Last x
                Just so -> Yield x (Outer so)
            Yield x s2 -> Yield x (Inner (Unfold s2 g) mso)
            -}

{-# INLINE concatMapNeNu #-}
concatMapNeNu :: (a -> NeNuList b) -> NeNuList a -> NeNuList b
concatMapNeNu k (Unfold s0 f) = Unfold (Outer s0) $ \ s ->
    case s of
        Outer s' -> case f s' of
            Last a -> case k a of
                Unfold s1 g -> case g s1 of
                    Last x     -> Last x
                    Yield x s2 -> Yield x (Inner (Unfold s2 g) Nothing)
            Yield a so -> case k a of
                Unfold s1 g -> case g s1 of
                    Last x     -> Yield x (Outer so)
                    Yield x s2 -> Yield x (Inner (Unfold s2 g) (Just so))
        Inner (Unfold s1 g) mso -> case g s1 of
            Last x     -> case mso of
                Nothing -> Last x
                Just so -> Yield x (Outer so)
            Yield x s2 -> Yield x (Inner (Unfold s2 g) mso)

{-
data Concat a s
    = Outer s
    | forall s' . Inner s' (s' -> Step a s') (Maybe s)

{-# INLINE concatNeNu #-}
concatNeNu :: forall a . NeNuList (NeNuList a) -> NeNuList a
concatNeNu (Unfold s0 f) = Unfold (Outer s0) $ \ s ->
    case s of
        Outer s' -> case f s' of
            Last (Unfold s1 g) -> case g s1 of
                Last x     -> Last x
                Yield x s2 -> Yield x (Inner s2 g Nothing)
            Yield (Unfold s1 g) so -> case g s1 of
                Last x     -> Yield x (Outer so)
                Yield x s2 -> Yield x (Inner s2 g (Just so))
        Inner s1 g mso -> case g s1 of
            Last x     -> case mso of
                Nothing -> Last x
                Just so -> Yield x (Outer so)
            Yield x s2 -> Yield x (Inner s2 g mso)

-}


{-
{-# INLINE concatNeNu #-}
concatNeNu :: forall a . NeNuList (NeNuList a) -> NeNuList a
concatNeNu (Unfold s0 f) = Unfold (Outer (f s0)) $ \ s ->
    case s of
        Outer (Last (Unfold s1 g)) -> case g s1 of
            Last x     -> Last x
            Yield x s2 -> Yield x (Inner s2 g Nothing)
        Outer (Yield (Unfold s1 g) so) -> case g s1 of
            Last x     -> Yield x (Outer (f so))
            Yield x s2 -> Yield x (Inner s2 g (Just (f so)))
        Inner s1 g Nothing -> case g s1 of
            Last x     -> Last x
            Yield x s2 -> Yield x (Inner s2 g Nothing)
        Inner s1 g (Just so) -> case g s1 of
            Last x     -> Yield x (Outer so)
            Yield x s2 -> Yield x (Inner s2 g (Just so))
            -}

{-
data Concat a s
    = Outer s
    | forall s' . Inner s' (s' -> Step a s')
    | forall s' . InnerThen s' (s' -> Step a s') s

{-# INLINE concatNeNu #-}
concatNeNu :: forall a . NeNuList (NeNuList a) -> NeNuList a
concatNeNu (Unfold s0 f) = Unfold (Outer (f s0)) $ \ s ->
    case s of
        Outer (Last (Unfold s1 g)) -> case g s1 of
            Last x     -> Last x
            Yield x s2 -> Yield x (Inner s2 g)
        Outer (Yield (Unfold s1 g) so) -> case g s1 of
            Last x     -> Yield x (Outer (f so))
            Yield x s2 -> Yield x (InnerThen s2 g (f so))
        Inner s1 g -> case g s1 of
            Last x     -> Last x
            Yield x s2 -> Yield x (Inner s2 g)
        InnerThen s1 g so -> case g s1 of
            Last x     -> Yield x (Outer so)
            Yield x s2 -> Yield x (InnerThen s2 g so)
            -}

{-# INLINE concatNeNu #-}
concatNeNu :: forall a . NeNuList (NeNuList a) -> NeNuList a
concatNeNu (Unfold s0 f) = Unfold (Just s0, Nothing) $ uncurry $ \ msa a ->
    case a of
        Nothing -> case msa of
            Nothing           -> error "!?"
            Just sa -> case f sa of
                Last (Unfold s g) -> case g s of
                    Last x     -> Last x
                    Yield x s' -> Yield x (Nothing, Just (Unfold s' g))
                Yield (Unfold s g) sa' -> case g s of
                    Last x     -> Yield x (Just sa', Nothing)
                    Yield x s' -> Yield x (Just sa', Just (Unfold s' g))
        Just (Unfold s g) -> case g s of
            Last x     -> case msa of
                Just sa -> Yield x (Just sa, Nothing)
                Nothing -> Last x
            Yield x s' -> Yield x (msa, Just (Unfold s' g))

{-# INLINE singletonNeNu #-}
singletonNeNu :: a -> NeNuList a
singletonNeNu x = Unfold () $ \ () -> Last x

instance Functor NeNuList where
  {-# INLINE fmap #-}
  fmap f (Unfold s g) = Unfold s $ \x -> case g x of
    Last a -> Last (f a)
    Yield a t -> Yield (f a) t

instance Monad NeNuList where
  {-# INLINE return #-}
  return = singletonNeNu
  {-# INLINE (>>=) #-}
  xs >>= f = concatNeNu (fmap f xs)
--   xs >>= f = concatMapNeNu f xs


{-# INLINE listToNeNu #-}
listToNeNu :: [a] -> NeNuList a
listToNeNu xs0 = Unfold xs0 go
  where
    go []     = error "listToNeNu: empty list!"
    go [x]    = Last x
    go (x:xs) = Yield x xs

{-# INLINE enumNeNu #-}
enumNeNu :: (Num a,Ord a) => a -> NeNuList a
enumNeNu to = Unfold 0 $ \n -> if n < to then Yield n (n+1) else Last n

{-# INLINE enumFromNeNu #-}
enumFromNeNu :: Num a => a -> NeNuList a
enumFromNeNu from = Unfold from $ \n -> Yield n (n+1)

{-# INLINE enumFromToNeNu #-}
enumFromToNeNu :: (Enum a,Ord a) => a -> a -> NeNuList a
enumFromToNeNu from to = Unfold from $ \ n -> if n < to then Yield n (succ n) else Last n

{-# INLINE enumFromThenToNeNu #-}
enumFromThenToNeNu :: (Num a,Ord a) => a -> a -> a -> NeNuList a
enumFromThenToNeNu from thn to = Unfold from $ \ n -> if n < to then Yield n (n + delta) else Last n
  where
    delta = thn - from

instance Comonad NeNuList where
  extract (Unfold s0 psi) = case psi s0 of
    Last x -> x
    Yield x _ -> x
  duplicate (Unfold s0 psi) = Unfold s0 go where
    go s = case psi s of
      Last x -> Last (singletonNeNu x)
      Yield _ t -> Yield (Unfold s psi) t

headNeNu :: NeNuList a -> a
headNeNu (Unfold s psi) = case psi s of
                          Last a -> a
                          Yield a _ -> a

-------------------------
-- Sinks

instance Foldable NeNuList where
  foldMap f = foldNeNu (\b a -> b `mappend` f a) mempty


{-# INLINE sumNeNu #-}
sumNeNu :: NeNuList Int -> Int
sumNeNu = foldNeNu (+) 0

-- 'foldl' (implemented with accumulator).  It's ok to use general
-- recursion here because it is the end of the pipeline.
{-# INLINE foldNeNu #-}
foldNeNu :: (b -> a -> b) -> b -> NeNuList a -> b
foldNeNu f k (Unfold s0 psi) = go k s0
  where go acc s = case psi s of
          Last x    -> f acc x
          Yield x t -> go (f acc x) t

{-

{-# INLINE stepToMaybe #-}
stepToMaybe :: Step t t1 -> Maybe (t, t1)
stepToMaybe Last = Nothing
stepToMaybe (Yield a s) = Just (a,s)

{-# INLINE maybeToStep #-}
maybeToStep :: Maybe (t, t1) -> Step t t1
maybeToStep Nothing = Last
maybeToStep (Just (a,s)) = Yield a s

{-# INLINE nuToList #-}
nuToList :: NeNuList a -> [a]
nuToList (Unfold s psi) = unfoldr (stepToMaybe . psi) s

{-# INLINE nuFromList #-}
nuFromList :: [a] -> NeNuList a
nuFromList = listToNu


{-# INLINE zipNu #-}
zipNu :: NeNuList a -> NeNuList b -> NeNuList (a,b)
zipNu = zipWithNu (,)

{-# INLINE zipWithNu #-}
zipWithNu :: (a -> b -> c) -> NeNuList a -> NeNuList b -> NeNuList c
zipWithNu f (Unfold s1 psi1) (Unfold s2 psi2) = Unfold (s1,s2) go
  where go ~(t1,t2) = case (psi1 t1,psi2 t2) of
           (Last,_) -> Last
           (_,Last) -> Last
           (Yield x u1,Yield y u2) -> Yield (f x y) (u1,u2)

{-# INLINE takeNu #-}
takeNu :: Int -> NeNuList a -> NeNuList a
takeNu n0 (Unfold s0 psi) = Unfold (n0,s0) (uncurry go) where
  go 0 _ = Last
  go n s = case psi s of
    Last -> Last
    Yield x t -> Yield x (n-1,t)

{-# INLINE takeWhileNu #-}
takeWhileNu :: (a -> Bool) -> NeNuList a -> NeNuList a
takeWhileNu p (Unfold s0 psi) = Unfold s0 go where
  go s = case psi s of
    Last -> Last
    Yield x t -> if p x then Yield x t else Last

{-# INLINE scanNu #-}
scanNu :: (b -> a -> b) -> b -> NeNuList a -> NeNuList b
scanNu f k (Unfold s0 psi) = Unfold (Just (k,s0)) go where
  go Nothing = Last
  go (Just (acc,s)) = case psi s of
    Last -> Yield acc Nothing
    Yield x t -> Yield acc (Just (f acc x,t))


{-# INLINE iterateNu #-}
iterateNu :: (a -> a) -> a -> NeNuList a
iterateNu f x0 = Unfold x0 go where
  go x = Yield x (f x)

{-
-- Do not fuse: use freeze
dropNu :: Int -> NeNuList a -> NeNuList a
dropNu n0 (Unfold s0 psi) = Unfold (go n0 s0) psi
  where go 0 s = s
        go n s = case psi s of
                      Yield _ t -> go (n-1) t
                      Last -> s

dropWhileNu :: (a -> Bool) -> NeNuList a -> NeNuList a
dropWhileNu p (Unfold s0 psi) = Unfold (go s0) psi
  where go s = case psi s of
                      Yield x t -> if p x then go t else t
                      Last -> s

splitAtNu :: Int -> NeNuList a -> (NeNuList a, NeNuList a)
splitAtNu n xs = (takeNu n xs,dropNu n xs)

spanNu :: (a -> Bool) -> NeNuList a -> (NeNuList a, NeNuList a)
spanNu p xs = (takeWhileNu p xs,dropWhileNu p xs)
-}


unfold :: (b -> Maybe (a, b)) -> b -> NeNuList a
unfold psi s = Unfold s (maybeToStep . psi)

repeatNu :: a -> NeNuList a
repeatNu a = Unfold () (const (Yield a ()))

cycleNu :: NeNuList a -> NeNuList a
cycleNu (Unfold s0 psi) = Unfold (s0,s0) psi'
  where psi' (s,s') =
          case psi s of
            Last -> case psi s' of
              Last -> Last -- exception empty list?
              Yield a s'' -> Yield a (s'',s')
            Yield a s'' -> Yield a (s'',s')

cycleNu' :: NeNuList a -> NeNuList a
cycleNu' (Unfold s0 psi) = case psi s0 of
  Last -> error "cycleNu': empty list"
  _ -> Unfold s0 psi' where
    psi' s = case psi s of
            Last -> psi s0
            y -> y

-- TODO: check if fusion happens in the above case.

-- intersperseNu see coutts & al


consNu :: a -> NeNuList a -> NeNuList a
consNu a0 (Unfold s0 psi) = Unfold Nothing psi'
  where psi' Nothing = Yield a0 (Just s0)
        psi' (Just s) = case psi s of
          Last -> Last
          Yield b s' -> Yield b (Just s')
{-
consNu :: a -> NeNuList a -> NeNuList a
consNu a0 (Unfold s0 psi) = Unfold (Just (a0,s0)) psi'
  where psi' Nothing = Last
        psi' (Just (a,s)) = case psi s of
          Last -> Yield a Nothing
          Yield b s' -> Yield a (Just (b,s'))
-}

nilNu :: NeNuList a
nilNu = Unfold () (const Last)

viewNu :: NeNuList a -> Step a (NeNuList a)
viewNu (Unfold s psi) = case psi s of
                             Last -> Last
                             Yield a s' -> Yield a (Unfold s' psi)


-}


