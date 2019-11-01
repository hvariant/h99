{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Q21_28 where

import Q11_20 hiding (main)

import System.Random hiding (split)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Array.IO as A
import qualified Data.Map.Strict as M
import Control.Arrow (first, second)

import Test.Hspec

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
  | n <= 0 = x:xs
  | otherwise = let (before, after) = split xs (n-1)
                 in before ++ [x] ++ after

range :: Int -> Int -> [Int]
range x y = [x..y]

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = map (xs !!) <$> replicateM n mr
  where mr = randomRIO (0, length xs - 1)

diffSelect :: Int -> Int -> IO [Int]
diffSelect 0 _ = return []
diffSelect n m
  | n == m = rndPermu [1..m]
  | otherwise = diffSelect' [1..m] m n
  -- lifted from https://wiki.haskell.org/99_questions/Solutions/23
  -- a really clever way to do deduplication in O(N)
  where diffSelect' :: [a] -> Int -> Int -> IO [a]
        diffSelect' _ _ 0 = pure []
        diffSelect' [] _ _ = pure []
        diffSelect' (x:xs) l n = randomRIO (0, l)
                              >>= \r -> if r < n
                                      then (x:) <$> diffSelect' xs (l-1) (n-1)
                                      else diffSelect' xs (l-1) n

-- fisher-yates with IOArray
rndPermu :: [a] -> IO [a]
rndPermu xs = do
  arr <- newArray l xs
  forM_ [1..l] $ \i -> do
    j <- randomRIO (1, i)
    vi <- A.readArray arr i
    vj <- A.readArray arr j
    A.writeArray arr j vi
    A.writeArray arr i vj
  A.getElems arr
  where
    l = length xs
    newArray :: Int -> [a] -> IO (A.IOArray Int a)
    newArray n = A.newListArray (1,n)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations 1 xs = map (:[]) xs
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

combinations' :: Int -> [a] -> [([a],[a])]
combinations' 0 _ = []
combinations' _ [] = []
combinations' 1 xs = map ((\(Just y, ys) -> ([y], ys)) . (`removeAt` xs)) $ [1..length xs]
combinations' k (x:xs) = map (first (x:)) (combinations' (k-1) xs) ++ map (second (x:)) (combinations' k xs)

group :: [Int] -> [a] -> [[[a]]]
group [] _ = []
group [n] xs = map (pure . fst) . combinations' n $ xs
group (n:ns) xs = concatMap (\(y, ys) -> map (y:) $ group ns ys) . combinations' n $ xs

lsort :: [[a]] -> [[a]]
lsort = L.sortBy key
    where key l1 l2 = compare (length l1) (length l2)

lfsort :: [[a]] -> [[a]]
lfsort xs = map snd . L.sortBy key $ subWithLen
    where subWithLen = map (\x -> (length x, x)) xs
          freqMap :: M.Map Int Int
          freqMap = M.fromListWith (+) $ map (second (const 1)) subWithLen
          freq l = fromMaybe 0 $ M.lookup l freqMap
          key (l1, _) (l2, _) = compare (freq l1) (freq l2)

main :: IO ()
main = hspec $ do
  describe "insertAt" $ do
    it "should work with valid indices" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
      insertAt 'X' "abcd" 1 `shouldBe` "Xabcd"
      insertAt 'X' "abcd" 4 `shouldBe` "abcXd"
      insertAt 'X' "abcd" 5 `shouldBe` "abcdX"
    it "should work with invalid indices" $ do
      insertAt 'X' "abcd" 100 `shouldBe` "abcdX"
      insertAt 'X' "abcd" (-100) `shouldBe` "Xabcd"

  describe "rndSelect" $ do
    it "should work" $ do
      rndSelect "abcdefgh" 3
        >>= \xs -> xs `shouldSatisfy` all (`elem` "abcdefgh")

  describe "diffSelect" $ do
    it "should work" $ do
      diffSelect 3 10
        >>= \xs -> xs `shouldSatisfy` all (\n -> n >= 1 && n <= 10)
      diffSelect 3 10
         >>= \xs -> xs `shouldSatisfy` all ((==1) . length) . L.group . L.sort
      diffSelect 10 10
         >>= \xs -> xs `shouldSatisfy` all ((==1) . length) . L.group . L.sort
      diffSelect 100 10
         >>= \xs -> xs `shouldSatisfy` all ((==1) . length) . L.group . L.sort
    it "should handle selecting all elements" $ do
      diffSelect 10 10
         >>= \xs -> length xs `shouldBe` 10

  describe "rndPermu" $ do
    it "should work" $ do
      xs <- liftIO (rndPermu "abcdef")
      L.sort xs `shouldBe` "abcdef"

  describe "combinations" $ do
    it "should work" $ do
      combinations 3 "abcdef" `shouldSatisfy` (==20) . length
      combinations 0 "abcdef" `shouldSatisfy` (==1) . length
      combinations 10 "abcdef" `shouldSatisfy` null

  describe "group" $ do
    it "should work" $ do
      group [2,3,4] ["A", "B", "C", "D", "E", "F", "G", "H", "I"]
        `shouldSatisfy` (==1260) . length
      group [2,2,5] ["A", "B", "C", "D", "E", "F", "G", "H", "I"]
        `shouldSatisfy` (==756) . length

  describe "lsort" $ do
    it "should work" $ do
      lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
        `shouldBe` ["o", "de", "de", "mn", "abc", "fgh", "ijkl"]

  describe "lfsort" $ do
    it "should work" $ do
      lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
        `shouldBe` ["ijkl", "o", "abc", "fgh", "de", "de", "mn"]
