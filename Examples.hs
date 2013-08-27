import Fusion
import Data.Foldable as F

fibs :: NuList Int
fibs = 0 `consNu'` (1 `consNu'` zipWithNu (+) (tailNu fibs) fibs)
-- loops


filterMax0 :: MuList Int -> (Int,MuList Int)
filterMax0 xs = (F.maximum ys,ys)
  where ys = filterMu (>10) xs
        

filterMax1 :: MuList Int -> (Int,MuList Int)
filterMax1 xs = F.foldr (\x (m,l) -> (max x m,consMu x l)) (0,nilMu) $ 
                filterMu (>10) xs
                
                