module Main where

import Fusion
import Data.Foldable as F
import System.Environment

ones :: NuList Int
ones = 1 `consNu` ones

-- fibs :: NuList Int
-- fibs = 0 `consNu'` (1 `consNu'` zipWithNu (+) (tailNu fibs) fibs)
-- loops

fibs' :: NuList Integer
fibs' = unfold (\(m,n) -> Just (m,(n,n+m))) (0,1)

example :: Int -> Int
example to = sumNu (enumFromToNu 0 to)

example2 :: Int -> Int
example2 to = F.sum (freeze (enumFromToNu 0 to))

example3 :: Int -> (Int,MuList Int)
example3 i = filterMax0 xs
  where
    xs = freeze (enumNu i)

filterMax0 :: MuList Int -> (Int,MuList Int)
filterMax0 xs = (F.maximum ys,ys)
  where ys = filterMu (>10) xs


example4 :: Int -> (Int,MuList Int)
example4 i = filterMax1 xs
  where
    xs = freeze (enumNu i)

filterMax1 :: MuList Int -> (Int,MuList Int)
filterMax1 xs = F.foldr (\x (m,l) -> (max x m,consMu x l)) (0,nilMu) $
                filterMu (>10) xs

example5 :: Int -> Bool
example5 x = F.and $ fmap (const False) $ filterMu (> x) $ freeze $ enumFromNu 0



main :: IO ()
main = do
    [arg] <- getArgs
    print (example (read arg))

