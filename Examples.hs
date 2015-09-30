module Main where

import Control.Fusion.List
import Data.Foldable as F
import System.Environment

ones :: NuList Int
ones = 1 `consNu` ones

-- fibs :: NuList Int
-- fibs = 0 `consNu'` (1 `consNu'` zipWithNu (+) (tailNu fibs) fibs)
-- loops

fibs' :: NuList Integer
fibs' = unfold (\(m,n) -> Just (m,(n,n+m))) (0,1)

example2 :: Int -> Int
example2 to = F.sum (enumFromToNu 0 to)

example3 :: Int -> (Int,MuList Int)
example3 i = filterMax0 $ loopStack (enumNu i)

filterMax0 :: MuList Int -> (Int,MuList Int)
filterMax0 xs = (F.maximum xs,ys)
  where ys = filterMu (>10) xs

example4 :: Int -> (Int,MuList Int)
example4 i = filterMax1 $ loopStack (enumNu i)

filterMax1 :: MuList Int -> (Int,MuList Int)
filterMax1 xs = F.foldr (\x (m,l) -> (max x m,consMu x l)) (0,nilMu) $ filterMu (>10) xs

filterMax2 :: NuList Int -> (Int,NuList Int)
filterMax2 xs = foldNu (\(acc,ys) x  -> if x > 10 then (max x acc, consNu x ys) else (max x acc,ys)) (0,nilNu) xs
   -- using lazy tuples is a disaster here.

example5 :: Int -> Bool
example5 x = F.and $ fmap (const False) $ filterMu (> x) $ loopStack $ enumFromNu 0

main :: IO ()
main = do
    [arg] <- getArgs
    print (example2 (read arg))

