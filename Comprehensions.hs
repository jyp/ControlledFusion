{-# LANGUAGE MonadComprehensions #-}
module Main where

import Fusion
import Data.Foldable as F hiding (fold,concatMap)
import qualified Tree as T
import Data.Monoid
import System.Environment

{- From

    Lists to Streams to Nothing at All
    Duncan Coutts, Roman Leshchinskiy, Don Stewart
-}
f :: Int -> Int
f n = F.foldl (+) 0
      [ k * m
      | k <- freeze (enumFromToNu 1 n)
      , m <- freeze (enumFromToNu 1 k)
      ]

g :: Int -> Int
g n = F.foldr (+) 0
      [ k * m
      | k <- freeze (enumFromToNu 1 n)
      , m <- freeze (enumFromToNu 1 k)
      ]

h :: Int -> Int
h n = sumNu
      [ k * m
      | k <- enumFromToNu 1 n
      , m <- enumFromToNu 1 k
      ]

mu_test_r :: Int -> Int
mu_test_r n = F.foldr (+) 0
      [ k * m
      | k <- enumMu 1 n
      , m <- enumMu 1 k
      ]

mu_test_l :: Int -> Int
mu_test_l n = F.foldl (+) 0
      [ k * m
      | k <- enumMu 1 n
      , m <- enumMu 1 k
      ]

t :: Int -> Int
t n = getSum $ F.foldMap id
    [ Sum (k * m)
    | k <- T.freeze (T.enumNuTree 1 n)
    , m <- T.freeze (T.enumNuTree 1 k)
    ]

l :: Int -> Int
l n = Prelude.sum
      [ k * m
      | k <- [1..n]
      , m <- [1..k]
      ]

main :: IO ()
main = do
    [arg] <- getArgs
    print (l (read arg))
    -- f 3000   : 0.759s
    -- g 3000   : 0.059s        <---- winner!!
    -- t 3000   : 0.119s
    -- h 3000   : 0.156s
    -- mu_test_r: 0.067s
    -- mu_test_l: 0.758s
    -- l        : 0.165s

{- l:
   1,080,832,640 bytes allocated in the heap
      60,841,016 bytes copied during GC
          44,416 bytes maximum residency (60 sample(s))
         189,760 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2010 colls,     0 par    0.05s    0.05s     0.0000s    0.0001s
  Gen  1        60 colls,     0 par    0.00s    0.00s     0.0000s    0.0001s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.12s  (  0.12s elapsed)
  GC      time    0.05s  (  0.05s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    0.17s  (  0.17s elapsed)

  %GC     time      28.6%  (28.7% elapsed)

  Alloc rate    8,930,623,216 bytes per MUT second

  Productivity  71.3% of total user, 71.6% of total elapsed


real	0m0.172s
user	0m0.167s
sys	0m0.003s
-}

{- g:
10136253375250
     104,011,200 bytes allocated in the heap
           7,528 bytes copied during GC
          44,416 bytes maximum residency (2 sample(s))
          21,120 bytes maximum slop
             103 MB total memory in use (1 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0         0 colls,     0 par    0.00s    0.00s     0.0000s    0.0000s
  Gen  1         2 colls,     0 par    0.01s    0.01s     0.0026s    0.0052s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.05s  (  0.05s elapsed)
  GC      time    0.01s  (  0.01s elapsed)
  EXIT    time    0.01s  (  0.01s elapsed)
  Total   time    0.06s  (  0.06s elapsed)

  %GC     time       8.3%  (8.4% elapsed)

  Alloc rate    1,983,262,986 bytes per MUT second

  Productivity  91.6% of total user, 92.7% of total elapsed


real	0m0.065s
user	0m0.047s
sys	0m0.017s
-}
