module Main where

import System.Environment

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

{-
$ time ./ComprehensionsFoldrBuild 3000 +RTS -K200M -s
10136253375250
     396,388,640 bytes allocated in the heap
          83,384 bytes copied during GC
          44,416 bytes maximum residency (2 sample(s))
          21,120 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0       764 colls,     0 par    0.00s    0.00s     0.0000s    0.0000s
  Gen  1         2 colls,     0 par    0.00s    0.00s     0.0001s    0.0001s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.05s  (  0.05s elapsed)
  GC      time    0.00s  (  0.00s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    0.06s  (  0.06s elapsed)

  %GC     time       4.7%  (4.7% elapsed)

  Alloc rate    7,403,040,409 bytes per MUT second

  Productivity  95.1% of total user, 96.8% of total elapsed


real	0m0.058s
user	0m0.053s
sys	    0m0.003s
-}
