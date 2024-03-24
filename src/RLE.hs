{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE(compress, uncompress) where

compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = compressHelper xs x 1
 where
   compressHelper :: Eq a => [a] -> a -> Int -> [(a, Int)]
   compressHelper [] currentRun runLength = [(currentRun, runLength)]
   compressHelper (y:ys) currentRun runLength
     | y == currentRun = compressHelper ys currentRun (runLength + 1)
     | otherwise       = (currentRun, runLength) : compressHelper ys y 1



uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just []
uncompress xs = Just $ concatMap (\(x, n) -> replicate n x) xs
