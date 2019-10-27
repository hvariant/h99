{-# OPTIONS_GHC -Wall #-}

import Data.Functor (($>))
import Data.List (foldl', group)

myLast :: [a] -> Maybe a
myLast = foldl' ($>) Nothing

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
isPalindrome xs = xs == (myReverse xs)

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

