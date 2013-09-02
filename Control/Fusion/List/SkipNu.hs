{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Control.Fusion.List.SkipNu where

import Data.Foldable
import Data.Monoid hiding (Last)

---------------
--  Skip Nu lists

data Step a s = Done | Skip s | Yield a s

data SkipNuList a where
  Unfold :: s -> (s -> Step a s) -> SkipNuList a

instance Show a => Show (SkipNuList a) where
  show = show . toList

{-# INLINE concatMapSkipNu #-}
concatMapSkipNu :: (a -> SkipNuList b) -> SkipNuList a -> SkipNuList b
concatMapSkipNu f (Unfold sa0 nexta) = Unfold (sa0, Nothing) next
  where
    {-# INLINE next #-}
    next (sa,Nothing) =
        case nexta sa of
            Done        -> Done
            Skip sa'    -> Skip (sa',Nothing)
            Yield a sa' -> Skip (sa',Just (f a))
    next (sa,Just (Unfold sb nextb)) =
        case nextb sb of
            Done        -> Skip    (sa,Nothing)
            Skip    sb' -> Skip    (sa,Just (Unfold sb' nextb))
            Yield b sb' -> Yield b (sa,Just (Unfold sb' nextb))

{-# INLINE singletonSkipNu #-}
singletonSkipNu :: a -> SkipNuList a
singletonSkipNu x = Unfold True $ \ s -> if s then Yield x False else Done

instance Functor SkipNuList where
  {-# INLINE fmap #-}
  fmap f (Unfold s g) = Unfold s $ \x -> case g x of
    Done      -> Done
    Skip t    -> Skip t
    Yield a t -> Yield (f a) t

instance Monad SkipNuList where
  {-# INLINE return #-}
  return = singletonSkipNu
  {-# INLINE (>>=) #-}
  xs >>= f = concatMapSkipNu f xs


{-# INLINE listToSkipNu #-}
listToSkipNu :: [a] -> SkipNuList a
listToSkipNu xs0 = Unfold xs0 go
  where
    go []     = Done
    go (x:xs) = Yield x xs

{-# INLINE enumSkipNu #-}
enumSkipNu :: (Num a,Ord a) => a -> SkipNuList a
enumSkipNu to = Unfold 0 $ \n -> if n <= to then Yield n (n+1) else Done

{-# INLINE enumFromSkipNu #-}
enumFromSkipNu :: Num a => a -> SkipNuList a
enumFromSkipNu from = Unfold from $ \n -> Yield n (n+1)

{-# INLINE enumFromToSkipNu #-}
enumFromToSkipNu :: (Enum a,Ord a) => a -> a -> SkipNuList a
enumFromToSkipNu from to = Unfold from $ \ n -> if n <= to then Yield n (succ n) else Done

{-# INLINE enumFromThenToSkipNu #-}
enumFromThenToSkipNu :: (Num a,Ord a) => a -> a -> a -> SkipNuList a
enumFromThenToSkipNu from thn to = Unfold from $ \ n -> if n <= to then Yield n (n + delta) else Done
  where
    delta = thn - from

-------------------------
-- Sinks

instance Foldable SkipNuList where
  foldMap f = foldSkipNu (\b a -> b `mappend` f a) mempty


{-# INLINE sumSkipNu #-}
sumSkipNu :: SkipNuList Int -> Int
sumSkipNu = foldSkipNu (+) 0

-- 'foldl' (implemented with accumulator).  It's ok to use general
-- recursion here because it is the end of the pipeline.
{-# INLINE foldSkipNu #-}
foldSkipNu :: (b -> a -> b) -> b -> SkipNuList a -> b
foldSkipNu f k (Unfold s0 psi) = go k s0
  where go acc s = case psi s of
          Done      -> acc
          Skip t    -> go acc t
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
nuToList :: SkipNuList a -> [a]
nuToList (Unfold s psi) = unfoldr (stepToMaybe . psi) s

{-# INLINE nuFromList #-}
nuFromList :: [a] -> SkipNuList a
nuFromList = listToNu


{-# INLINE zipNu #-}
zipNu :: SkipNuList a -> SkipNuList b -> SkipNuList (a,b)
zipNu = zipWithNu (,)

{-# INLINE zipWithNu #-}
zipWithNu :: (a -> b -> c) -> SkipNuList a -> SkipNuList b -> SkipNuList c
zipWithNu f (Unfold s1 psi1) (Unfold s2 psi2) = Unfold (s1,s2) go
  where go ~(t1,t2) = case (psi1 t1,psi2 t2) of
           (Last,_) -> Last
           (_,Last) -> Last
           (Yield x u1,Yield y u2) -> Yield (f x y) (u1,u2)

{-# INLINE takeNu #-}
takeNu :: Int -> SkipNuList a -> SkipNuList a
takeNu n0 (Unfold s0 psi) = Unfold (n0,s0) (uncurry go) where
  go 0 _ = Last
  go n s = case psi s of
    Last -> Last
    Yield x t -> Yield x (n-1,t)

{-# INLINE takeWhileNu #-}
takeWhileNu :: (a -> Bool) -> SkipNuList a -> SkipNuList a
takeWhileNu p (Unfold s0 psi) = Unfold s0 go where
  go s = case psi s of
    Last -> Last
    Yield x t -> if p x then Yield x t else Last

{-# INLINE scanNu #-}
scanNu :: (b -> a -> b) -> b -> SkipNuList a -> SkipNuList b
scanNu f k (Unfold s0 psi) = Unfold (Just (k,s0)) go where
  go Nothing = Last
  go (Just (acc,s)) = case psi s of
    Last -> Yield acc Nothing
    Yield x t -> Yield acc (Just (f acc x,t))


{-# INLINE iterateNu #-}
iterateNu :: (a -> a) -> a -> SkipNuList a
iterateNu f x0 = Unfold x0 go where
  go x = Yield x (f x)

{-
-- Do not fuse: use freeze
dropNu :: Int -> SkipNuList a -> SkipNuList a
dropNu n0 (Unfold s0 psi) = Unfold (go n0 s0) psi
  where go 0 s = s
        go n s = case psi s of
                      Yield _ t -> go (n-1) t
                      Last -> s

dropWhileNu :: (a -> Bool) -> SkipNuList a -> SkipNuList a
dropWhileNu p (Unfold s0 psi) = Unfold (go s0) psi
  where go s = case psi s of
                      Yield x t -> if p x then go t else t
                      Last -> s

splitAtNu :: Int -> SkipNuList a -> (SkipNuList a, SkipNuList a)
splitAtNu n xs = (takeNu n xs,dropNu n xs)

spanNu :: (a -> Bool) -> SkipNuList a -> (SkipNuList a, SkipNuList a)
spanNu p xs = (takeWhileNu p xs,dropWhileNu p xs)
-}


unfold :: (b -> Maybe (a, b)) -> b -> SkipNuList a
unfold psi s = Unfold s (maybeToStep . psi)

repeatNu :: a -> SkipNuList a
repeatNu a = Unfold () (const (Yield a ()))

cycleNu :: SkipNuList a -> SkipNuList a
cycleNu (Unfold s0 psi) = Unfold (s0,s0) psi'
  where psi' (s,s') =
          case psi s of
            Last -> case psi s' of
              Last -> Last -- exception empty list?
              Yield a s'' -> Yield a (s'',s')
            Yield a s'' -> Yield a (s'',s')

cycleNu' :: SkipNuList a -> SkipNuList a
cycleNu' (Unfold s0 psi) = case psi s0 of
  Last -> error "cycleNu': empty list"
  _ -> Unfold s0 psi' where
    psi' s = case psi s of
            Last -> psi s0
            y -> y

-- TODO: check if fusion happens in the above case.

-- intersperseNu see coutts & al


consNu :: a -> SkipNuList a -> SkipNuList a
consNu a0 (Unfold s0 psi) = Unfold Nothing psi'
  where psi' Nothing = Yield a0 (Just s0)
        psi' (Just s) = case psi s of
          Last -> Last
          Yield b s' -> Yield b (Just s')
{-
consNu :: a -> SkipNuList a -> SkipNuList a
consNu a0 (Unfold s0 psi) = Unfold (Just (a0,s0)) psi'
  where psi' Nothing = Last
        psi' (Just (a,s)) = case psi s of
          Last -> Yield a Nothing
          Yield b s' -> Yield a (Just (b,s'))
-}

nilNu :: SkipNuList a
nilNu = Unfold () (const Last)

viewNu :: SkipNuList a -> Step a (SkipNuList a)
viewNu (Unfold s psi) = case psi s of
                             Last -> Last
                             Yield a s' -> Yield a (Unfold s' psi)


-}


