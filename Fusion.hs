{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

-- (c) JP Bernardy; GPL-licensed.

module Fusion where

import Data.List (unfoldr)
import Control.Comonad

-------------
-- Mu lists

-- Fα = 1 + a × α
-- MuList = μα. (Fα → α) → α
newtype MuList a = Build {fold :: forall x. (a -> x -> x) -> x -> x}
  
instance Show a => Show (MuList a) where                   
  show = show . muToList
                   
instance Functor MuList where                   
  fmap f (Build g) = Build $ \cons nil -> g (\h t -> cons (f h) t) nil

filterMu :: (a -> Bool) -> MuList a -> MuList a
filterMu p (Build g) = Build $ \cons nil -> g (\h t -> if p h then cons h t else t) nil

concatMu :: MuList (MuList a) -> MuList a                                            
concatMu (Build g) = Build $ \cons nil -> g (\(Build h) t -> h cons t) nil  

instance Monad MuList where
  return x = Build $ \cons nil -> cons x nil
  x >>= f = concatMu (fmap f x) -- efficient!

sumMu :: MuList Int -> Int
sumMu (Build g) = g (+) 0

takeWhileMu :: (a -> Bool) -> MuList a -> MuList a
takeWhileMu p (Build g) = Build $ \cons nil -> g (\h t -> if p h then cons h t else nil) nil

takeMu :: Int -> MuList a -> MuList a
takeMu n (Build g) = Build $ \cons nil -> g (takeFB cons nil) (const nil) n

takeFB :: (a -> b -> b) -> b -> a -> (Int -> b) -> Int -> b
takeFB c n x xs m | m <= 1  = x `c` n
                  | otherwise = x `c` xs (m - 1)

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

-- NuList = να. (α → Fα) × α

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
  (>>=) = flip concatMapNu -- Not *reall*y a monad: uses general recursion.
-}
  
data Step a s = Done | Yield a s

stepToMaybe :: Step t t1 -> Maybe (t, t1)
stepToMaybe Done = Nothing
stepToMaybe (Yield a s) = Just (a,s)

nuToList :: NuList a -> [a]
nuToList (Unfold s psi) = unfoldr (stepToMaybe . psi) s

listToNu :: [a] -> NuList a
listToNu xs0 = Unfold xs0 go
  where go [] = Done
        go (x:xs) = Yield x xs                                            

instance Functor NuList where                   
  fmap f (Unfold s g) = Unfold s $ \x -> case g x of
    Done-> Done
    Yield a t -> Yield (f a) t

zipNu :: NuList a -> NuList b -> NuList (a,b)
zipNu (Unfold s1 psi1) (Unfold s2 psi2) = Unfold (s1,s2) go 
  where go (t1,t2) = case (psi1 t1,psi2 t2) of
           (Done,_) -> Done
           (_,Done) -> Done
           (Yield x u1,Yield y u2) -> Yield (x,y) (u1,u2)

takeNu :: Int -> NuList a -> NuList a
takeNu n0 (Unfold s0 psi) = Unfold (n0,s0) (uncurry go) where
  go 0 _ = Done
  go n s = case psi s of
    Done -> Done
    Yield x t -> Yield x (n-1,t)

enumNu :: Int -> NuList Int
enumNu to = Unfold 0 $ \n -> if n < to then Yield n (n+1) else Done

enumFromNu :: Int -> NuList Int
enumFromNu from = Unfold from $ \n -> Yield n (n+1)


sumNu :: NuList Int -> Int
sumNu = foldNu (+) 0

-- 'foldl' (implemented with accumulator).  It's ok to use general
-- recursion here because it is the end of the pipeline.
foldNu :: (b -> a -> b) -> b -> NuList a -> b
foldNu f k (Unfold s0 psi) = go k s0
  where go acc s = case psi s of
          Done -> acc
          Yield x t  -> go (f acc x) t
          
scanNu :: (b -> a -> b) -> b -> NuList a -> NuList b
scanNu f k (Unfold s0 psi) = Unfold (Just (k,s0)) go where
  go Nothing = Done
  go (Just (acc,s)) = case psi s of
    Done -> Yield acc Nothing
    Yield x t -> Yield acc (Just (f acc x,t))
    

instance Comonad NuList where
  extract (Unfold s0 psi) = case psi s0 of
    Done -> error "extract: empty NuList"
    Yield x _ -> x
  duplicate (Unfold s0 psi) = Unfold s0 go where
    go s = case psi s of
      Done -> Done
      Yield _ t -> Yield (Unfold s psi) t



-------------------------
-- Conversion functions

-- No general rec: can be fused away!
thaw :: MuList a -> NuList a
thaw (Build g) = Unfold (g (:) []) go
  where go [] = Done
        go (x:xs) = Yield x xs
        
-- General rec: cannot be fused!
freeze :: forall a. NuList a -> MuList a 
freeze (Unfold s0 psi) = Build $ \cons nil ->
  let go s = case psi s of
          Done -> nil
          Yield a s' -> cons a (go s')
  in go s0




