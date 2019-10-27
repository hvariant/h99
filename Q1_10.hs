{-# OPTIONS_GHC -Wall #-}

module Q1_10 where

import Data.List (foldl', group)
import Test.Hspec
import Test.QuickCheck

myLast :: [a] -> Maybe a
myLast = foldl' (const Just) Nothing

myButLast :: [a] -> Maybe a
myButLast = fst . foldl' f (Nothing, Nothing)
  where f (_, b) x = (b, Just x)

elementAt :: [a] -> Int -> Maybe a
elementAt xs n
  | n <= 0 = Nothing
  | otherwise = snd . foldl' f (1, Nothing) $ xs
  where f (i, y) x
          | n == i    = (i+1, Just x)
          | otherwise = (i+1, y)

myLength :: [a] -> Int
myLength = foldr (const (+1)) 0

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

compress :: (Eq a) => [a] -> [a]
compress = map head . group

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (eq, neq) = span (== x) xs
               in (x:eq) : pack neq

encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\xs ->  (myLength xs, head xs)) . pack

main :: IO ()
main = hspec $ do
  describe "myLast" $ do
    it "should handle nonempty lists" $ do
      myLast "abcd" `shouldBe` Just 'd'
    it "should handle empty list" $ do
      myLast "" `shouldBe` Nothing

  describe "myButLast" $ do
    it "should handle lists with length >= 2" $ do
      myButLast "Shrek" `shouldBe` Just 'e'
      myButLast "IO" `shouldBe` Just 'I'
    it "should handle lists with length < 2" $ do
      myButLast "K" `shouldBe` Nothing
      myButLast "" `shouldBe` Nothing

  describe "elementAt" $ do
    it "should handle valid indices" $ do
      elementAt "haskell" 5 `shouldBe` Just 'e'
      elementAt "notch" 2 `shouldBe` Just 'o'
    it "should handle invalid indices" $ do
      elementAt "123" 0 `shouldBe` Nothing
      elementAt "rhythm" 10 `shouldBe` Nothing

  describe "myLength" $ do
    it "should be the same as length" $
      property $ \xs -> length xs == myLength (xs :: [Int])

  describe "myReverse" $ do
    it "should reverse the list back" $
      property $ \xs -> myReverse (myReverse xs) == (xs :: [Int])

  describe "flatten" $ do
    it "should work" $ do
      flatten (Elem 5) `shouldBe` ([5]::[Int])
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        `shouldBe` ([1,2,3,4,5]::[Int])
      flatten (List []) `shouldBe` ([]::[Int])

  describe "compress" $ do
    it "should work" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

  describe "pack" $ do
    it "should work" $ do
      pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  describe "encode" $ do
    it "should work" $ do
      encode "aaaabccaadeeee" `shouldBe`
        [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
