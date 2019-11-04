{-# OPTIONS_GHC -Wall #-}

module Q80_89 where

import Control.Arrow (second)
import Data.Maybe (isJust, fromMaybe)
import Data.Bool (bool)
import Data.Tuple (swap)
import Data.List (nubBy, sort, sortBy, find, foldl')
import qualified Data.Set as S
import qualified Data.Map as M

import Safe.Foldable (minimumByMay)
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

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap chapo (permutations xs)
  where
    chapo [] = [[x]]
    chapo (y:ys) = [(x:y:ys)] ++ (fmap (y:) $ chapo ys)

iso :: (Ord a) => Graph a -> Graph a -> Maybe [(a, a)]
iso g1@(Graph nodes1 edges1) g2@(Graph nodes2 edges2)
  | length nodes1 /= length nodes2 = Nothing
  | length edges1 /= length edges2 = Nothing
  | otherwise = find isIso mappings
    where
      perms1 = permutations nodes1
      mappings = fmap (zip nodes2) perms1
      graphEq (Graph ns1 es1) (Graph ns2 es2) =
        (S.fromList ns1) == (S.fromList ns2) &&
        (S.fromList es1) == (S.fromList es2)
      mapGraph mapping (Graph nodes edges) =
        Graph (fmap (mapping M.!) nodes) (fmap (\(a, b) -> (mapping M.! a, mapping M.! b)) edges)
      isIso mapping = (mapGraph (M.fromList mapping) g2) `graphEq` g1

adjacentNodes :: (Ord a) => Graph a -> MM.MultiMap a a
adjacentNodes = MM.fromList
              . concatMap (\(a, as) -> zip (repeat a) as)
              . getArcs
              . graphToAdj
  where getArcs (Adj arcs) = arcs

kColor :: Ord a => Graph a -> [(a, Int)]
kColor g@(Graph nodes edges) = M.toList
                             . foldl' bestColor M.empty
                             $ sortedNodes
  where
    bestColor m n = M.insert n (findBestColor 1) m
      where findBestColor i
              | hasConflict = findBestColor (i+1)
              | otherwise = i
              where adjNodes = n `MM.lookup` (adjacentNodes g)
                    hasConflict = any (\n' -> fromMaybe False $ (== i) <$> (n' `M.lookup` m)) adjNodes

    sortedNodes = fmap snd
                . sortBy (\(a,_) (b,_) -> compare b a)
                . zip (fmap degree nodes)
                $ nodes

    degree node =
      length $ filter (\(a, b) -> a == node || b == node) edges

depthFirst' :: Ord a => S.Set a -> a -> [a] -> MM.MultiMap a a -> (S.Set a, [a])
depthFirst' vs cur acc adjNodes =
  foldl' step (cur `S.insert` vs, cur : acc) nexts
  where nexts = filter (not . (`S.member` vs)) (cur `MM.lookup` adjNodes)
        step (vs', acc') n
          | n `S.member` vs' = (vs', acc')
          | otherwise = depthFirst' vs' n acc' adjNodes

depthFirst :: Ord a => Graph a -> a -> [a]
depthFirst g s = reverse
               . snd
               . depthFirst' S.empty s []
               $ adjNodes
  where adjNodes = adjacentNodes g

connectedComponents :: Ord a => Graph a -> [[a]]
connectedComponents g@(Graph ns _) = snd . foldr step (S.empty, []) $ ns
  where adjNodes = adjacentNodes g
        step n (vs, comps)
          | n `S.member` vs = (vs, comps)
          | otherwise = second (:comps) $ depthFirst' vs n [] adjNodes


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
      paths (1::Int) 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldSatisfy`
            (\ps -> setEq ps [[1,2,3,4],[1,3,4]])
      paths (1::Int) 1 [(1,2),(2,3),(3,1),(3,4),(4,2),(5,6)] `shouldSatisfy`
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

  describe "permutations" $ do
    it "works" $ do
      length (permutations ([1..5]::[Int])) `shouldBe` product ([1..5]::[Int])

  describe "iso" $ do
    it "works" $ do
      let graphG1 = Graph ([1,2,3,4,5,6,7,8]::[Int])
                          [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
          graphH1 = Graph ([1,2,3,4,5,6,7,8]::[Int])
                          [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
      iso graphG1 graphH1 `shouldSatisfy` isJust

  describe "kColor" $ do
    it "works" $ do
      let g = Graph "abcdefghij"
                    [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),
                     ('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),
                     ('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
      kColor g `shouldBe` [('a',1),('b',2),('c',1),('d',2),('e',3),
                           ('f',2),('g',1),('h',3),('i',3),('j',2)]

  describe "depthFirst" $ do
    it "works" $ do
      let setEq xs ys = (sort xs) == (sort ys)
      let g = Graph [1,2,3,4,5,6,7] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]
      depthFirst g (1::Int) `shouldSatisfy` \s -> s `setEq` [1,2,3,4,5]
      depthFirst g (2::Int) `shouldSatisfy` \s -> s `setEq` [1,2,3,4,5]
      depthFirst g (3::Int) `shouldSatisfy` \s -> s `setEq` [1,2,3,4,5]
      depthFirst g (4::Int) `shouldSatisfy` \s -> s `setEq` [1,2,3,4,5]
      depthFirst g (5::Int) `shouldSatisfy` \s -> s `setEq` [1,2,3,4,5]
      depthFirst g (6::Int) `shouldSatisfy` \s -> s `setEq` [6,7]
      depthFirst g (7::Int) `shouldSatisfy` \s -> s `setEq` [6,7]

  describe "connectedComponents" $ do
    it "works" $ do
      let setSetEq xs ys = (sort . fmap sort $ xs) == (sort . fmap sort $ ys)
      let g = Graph ([1,2,3,4,5,6,7]::[Int]) [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]
      connectedComponents g `shouldSatisfy` \ss -> setSetEq ss [[1,2,3,4,5],[6,7]]
