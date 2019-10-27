{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Q21_28 where

import Q11_20
import System.Random hiding (split)
import Data.List (sortBy)
import Control.Monad (replicateM, forM_)
import qualified Data.Array.IO as A
import qualified Data.Map.Strict as M
import Control.Arrow (first, second)

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
  | n <= 0 = xs
  | otherwise = let (before, after) = split xs (n-1)
                 in before ++ [x] ++ after

range :: Int -> Int -> [Int]
range x y = [x..y]

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = map (xs !!) <$> replicateM n mr
  where mr = randomRIO (0, (length xs) - 1)

diff_select :: Int -> Int -> IO [Int]
diff_select 0 _ = return []
diff_select n m
  | n == m = pure [1..m]
  | otherwise = diff_select' [1..m] m n
  -- lifted from https://wiki.haskell.org/99_questions/Solutions/23
  -- a really clever way to do deduplication in O(N)
  where diff_select' :: [a] -> Int -> Int -> IO [a]
        diff_select' _ _ 0 = pure []
        diff_select' [] _ _ = pure []
        diff_select' (x:xs) l n = randomRIO (0, l)
                              >>= \r -> if r < n
                                      then (x:) <$> diff_select' xs (l-1) (n-1)
                                      else diff_select' xs (l-1) n

-- fisher-yates with IOArray
rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
  arr <- newArray l xs
  forM_ [1..l] $ \i -> do
    j <- randomRIO (1, i)
    vi <- (A.readArray arr i)
    vj <- (A.readArray arr j)
    A.writeArray arr j vi
    A.writeArray arr i vj
  A.getElems arr
  where
    l = length xs
    newArray :: Int -> [a] -> IO (A.IOArray Int a)
    newArray n xs = A.newListArray (1,n) xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations 1 xs = map (\x -> [x]) xs
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

combinations' :: Int -> [a] -> [([a],[a])]
combinations' 0 _ = []
combinations' _ [] = []
combinations' 1 xs = map (\(Just y, ys) -> ([y], ys)) . map (`removeAt` xs) $ [1..length xs]
combinations' k (x:xs) = map (first (x:)) (combinations' (k-1) xs) ++ map (second (x:)) (combinations' k xs)

group :: [Int] -> [a] -> [[[a]]]
group [] _ = []
group (n:[]) xs = map pure . map fst . combinations' n $ xs
group (n:ns) xs = concatMap (\(y, ys) -> map (y:) $ group ns ys) . combinations' n $ xs

lsort :: [[a]] -> [[a]]
lsort = sortBy key
    where key l1 l2 = compare (length l1) (length l2)

lfsort :: [[a]] -> [[a]]
lfsort xs = map snd . sortBy key $ subWithLen
    where subWithLen = map (\x -> (length x, x)) xs
          freqMap :: M.Map Int Int
          freqMap = M.fromListWith (+) $ map (second (const 1)) subWithLen
          freq l = maybe 0 id $ M.lookup l freqMap
          key (l1, _) (l2, _) = compare (freq l1) (freq l2)
