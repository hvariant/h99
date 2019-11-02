{-# OPTIONS_GHC -Wall #-}

module Q80_89 where

import Data.Bool (bool)
import Data.Tuple (swap)
import Data.List (nubBy, sort)
import Safe.Foldable (minimumByMay)
import qualified Data.Set as S
import qualified Data.MultiMap as MM
import Q21_28 (combinations)

import Prelude hiding (cycle)

import Test.Hspec

data Graph a = Graph [a] [(a, a)]
  deriving (Show, Eq)

newtype Adj a = Adj [(a, [a])]
  deriving (Show, Eq)

graphToAdj :: (Eq a) => Graph a -> Adj a
graphToAdj = Adj . convert
  where convert (Graph [] _) = []
        convert (Graph (a:as) edges) =
          let toEdges   = filter ((== a) . fst) edges
              fromEdges = filter ((== a) . snd) edges
           in (a, fmap snd toEdges ++ fmap fst fromEdges)
              : convert (Graph as edges)

adjToGraph :: (Eq a) => Adj a -> Graph a
adjToGraph (Adj adj) = Graph nodes edges
  where nodes = fmap fst adj
        edges = nubBy dedup . concatMap unwind $ adj
        dedup (a, b) (c, d) = (a == c && b == d)
                           || (a == d && b == c)
        unwind (n, ns') = zip (repeat n) ns'

paths :: Ord a => a -> a -> [(a , a)] -> [[a]]
paths s t edges = (s:) <$> search s t S.empty []
  where edgesMap = MM.fromList edges
        search x y vs path =
          let adjs' = MM.lookup x edgesMap
              adjs = filter (\u -> not $ u `S.member` vs) adjs'
              r = concatMap (\u -> search u y (u `S.insert` vs) (u:path)) adjs
           in if x == y
              then reverse path : r
              else r

cycle :: Ord a => a -> [(a, a)] -> [[a]]
cycle s edges = search s S.empty [s]
  where edgesMap = MM.fromList edges
        search cur vs path =
          let adjs' = MM.lookup cur edgesMap
              adjs = S.fromList $ filter (\u -> not (u `S.member` vs) || u == s) adjs'
              ret = concatMap (\u -> search u (u `S.insert` vs) (u:path)) adjs
           in if s `S.member` adjs
              then reverse (s:path) : ret
              else ret

k4 :: Graph Char
k4 = Graph "abcd"
       [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

spanTrees :: Ord a => Graph a -> [Graph a]
spanTrees g@(Graph nodes edges)
    | length edges < length nodes - 1 = []
    | length edges == length nodes - 1 = bool [] [g] isConnected
    | otherwise = concatMap spanTrees (Graph nodes <$> combinations (length nodes - 1) edges)
  where
    doubleArcs = edges ++ fmap swap edges
    paths' n v = paths n v doubleArcs

    isConnected = search nodes
      where search [] = True
            search (n:ns') =
              all (not . null . paths' n) ns' && search ns'

prim :: Ord a => [a] -> [(a, a, Int)] -> Maybe [(a, a, Int)]
prim [] _ = Just []
prim _ [] = Nothing
prim (n:ns) es = search (S.singleton n) (S.fromList ns) es
  where search :: Ord a => S.Set a -> S.Set a -> [(a, a, Int)] -> Maybe [(a, a, Int)]
        search seen unseen edges
           | S.size unseen == 0 = Just []
           | otherwise =
             let isBridgeEdge (x, y, _) =
                   (x `S.member` seen) && (y `S.member` unseen) || (y `S.member` seen) && (x `S.member` unseen)
                 byWeight (_, _, w1) (_, _, w2) = compare w1 w2
                 minEdgeMay = minimumByMay byWeight . filter isBridgeEdge $ edges
              in case minEdgeMay of
                   Nothing -> Nothing
                   Just (x, y, w) -> ((x, y, w):)
                                 <$> search (x `S.insert` (y `S.insert` seen))
                                            (x `S.delete` (y `S.delete` unseen)) edges

main :: IO ()
main = hspec $ do
  describe "graphToAdj | adjToGraph" $ do
    it "works" $ do
      let gNodes = "bcdfghk"
          gEdges = [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
          g = Graph gNodes gEdges
          setEq xs ys = (sort xs) == (sort ys)
      (adjToGraph . graphToAdj $ g) `shouldSatisfy`
        \(Graph nodes edges) -> nodes `setEq` gNodes && edges `setEq` gEdges

  describe "paths" $ do
    it "works" $ do
      let setEq xs ys = (sort xs) == (sort ys)
       in paths (1::Int) 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldSatisfy`
            (\ps -> setEq ps [[1,2,3,4],[1,3,4]])
      let setEq xs ys = (sort xs) == (sort ys)
       in paths (1::Int) 1 [(1,2),(2,3),(3,1),(3,4),(4,2),(5,6)] `shouldSatisfy`
            (\ps -> setEq ps [[1], [1,2,3,1]])
      paths (2::Int) 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe` []

  describe "cycle" $ do
    it "works" $ do
      cycle (2::Int) [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe`
        [[2,3,4,2]]
      cycle (1::Int) [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe` []

  describe "spanTrees" $ do
    it "works" $ do
      length (spanTrees k4) `shouldBe` 16
      spanTrees (Graph "ab" []) `shouldBe` []

  describe "prim" $ do
    it "works" $ do
      prim ([]::String) [] `shouldBe` Just []
      let setEq xs ys = (sort xs) == (sort ys)
          nodes = [1,2,3,4,5] :: [Int]
          edges :: [(Int, Int, Int)]
          edges = [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
       in prim nodes edges `shouldSatisfy`
            \(Just p) -> p `setEq` [(1,2,12),(1,3,34),(2,4,55),(2,5,32)]
