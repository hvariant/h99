{-# OPTIONS_GHC -Wall #-}

module Q11_20 where

import Q1_10 hiding (main)
import Test.Hspec
import Test.QuickCheck

import Data.List (group)

data Cell a = Multiple Int a | Single a deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [Cell a]
encodeModified = map f . encode
  where f (1, a) = Single a
        f (n, a) = Multiple n a

decodeModified :: [Cell a] -> [a]
decodeModified = concatMap unfold
    where unfold (Single x) = [x]
          unfold (Multiple n x) = replicate n x

encodeDirect :: (Eq a) => [a] -> [Cell a]
encodeDirect = map f . group
  where f (a:[]) = Single a
        f as@(a:_) = Multiple (length as) a
        f [] = error "impossible"

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> replicate n x) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n
  | n <= 0 = xs
  | otherwise = take (n-1) xs ++ dropEvery (drop n xs) n

split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j-i'+1) . drop (i-1) $ xs
  where i' = max i 1

rotate :: [a] -> Int -> [a]
rotate xs n = (drop n' xs) ++ (take n' xs)
  where n' = n `mod` (length xs)

removeAt :: Int -> [a] -> (Maybe a,[a])
removeAt _ [] = (Nothing, [])
removeAt 1 (x:xs) = (Just x,xs)
removeAt n (x:xs) = let (r, rs) = removeAt (n-1) xs in
                        (r, x:rs)

main :: IO ()
main = hspec $ do
  describe "encodeModified" $ do
    it "should work" $ do
      encodeModified "aaaabccaadeeee" `shouldBe`
        [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

  describe "decodeModified" $ do
    it "should work" $ do
      decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c',
                      Multiple 2 'a', Single 'd', Multiple 4 'e']
        `shouldBe` "aaaabccaadeeee"

  describe "encodeDirect" $ do
    it "should work" $ do
      encodeDirect "aaaabccaadeeee"
        `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c',
                    Multiple 2 'a',Single 'd',Multiple 4 'e']

  describe "dupli" $ do
    it "should work" $ property $
      \xs -> let ys = pack (xs :: [Int])
                 ys' = pack $ dupli (xs :: [Int])
                 p y y' = 2 * (length y) == length y'
              in and $ zipWith p ys ys'

  describe "repli" $ do
    it "should work" $ property $
      \xs -> let ys = pack (xs :: [Int])
                 ys' = pack $ repli (xs :: [Int]) 4
                 p y y' = 4 * (length y) == length y'
              in and $ zipWith p ys ys'

  describe "dropEvery" $ do
    it "should work" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
      dropEvery "abcdefghik" 1 `shouldBe` ""
      dropEvery "abcdefghik" 2 `shouldBe` "acegi"
      dropEvery "abcdefghik" 0 `shouldBe` "abcdefghik"

  describe "split" $ do
    it "should work" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  describe "slice" $ do
    it "should work" $ do
      slice "abcdefghik" 3 7 `shouldBe` "cdefg"
      slice "abcdefghik" 0 7 `shouldBe` "abcdefg"
      slice "abcdefghik" 3 100 `shouldBe` "cdefghik"
      slice "abcdefghik" 0 100 `shouldBe` "abcdefghik"

  describe "rotate" $ do
    it "should work" $ do
      rotate "abcdefgh" 3 `shouldBe` "defghabc"
      rotate "abcdefgh" (-2) `shouldBe` "ghabcdef"
      rotate "abcdefgh" 0 `shouldBe` "abcdefgh"

  describe "removeAt" $ do
    it "should work with valid indices" $ do
      removeAt 2 "abcd" `shouldBe` (Just 'b', "acd")
    it "should work with invalid indices" $ do
      removeAt 0 "abcd" `shouldBe` (Nothing, "abcd")
      removeAt 100 "abcd" `shouldBe` (Nothing, "abcd")
