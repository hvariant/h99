{-# OPTIONS_GHC -Wall #-}

module Q11_20 where

import Q1_10

import Data.List (group)

data Cell a = Multiple Int a | Single a deriving (Show)

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
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j-i+1) . drop (i-1) $ xs

rotate :: [a] -> Int -> [a]
rotate xs n = (drop n' xs) ++ (take n' xs)
  where n' = n `mod` (length xs)

removeAt :: Int -> [a] -> (Maybe a,[a])
removeAt _ [] = (Nothing, [])
removeAt 1 (x:xs) = (Just x,xs)
removeAt n (x:xs) = let (r, rs) = removeAt (n-1) xs in
                        (r, x:rs)
