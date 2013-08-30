module Main where

import Fusion
import Data.Foldable as F
import System.Environment

fibs :: NuList Int
fibs = 0 `consNu'` (1 `consNu'` zipWithNu (+) (tailNu fibs) fibs)
-- loops

example :: Int -> Int
example to = sumNu (enumFromToNu 0 to)

example2 :: Int -> Int
example2 to = F.sum (freeze (enumFromToNu 0 to))

example3 :: Int -> (Int,MuList Int)
example3 i = filterMax0 xs
  where
    xs = freeze (enumNu i)

example4 :: Int -> (Int,MuList Int)
example4 i = filterMax1 xs
  where
    xs = freeze (enumNu i)


filterMax0 :: MuList Int -> (Int,MuList Int)
filterMax0 xs = (F.maximum ys,ys)
  where ys = filterMu (>10) xs


filterMax1 :: MuList Int -> (Int,MuList Int)
filterMax1 xs = F.foldr (\x (m,l) -> (max x m,consMu x l)) (0,nilMu) $
                filterMu (>10) xs



main :: IO ()
main = do
    [arg] <- getArgs
    print (example (read arg))

