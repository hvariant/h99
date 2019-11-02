{-# OPTIONS_GHC -Wall #-}

module Q80_89 where

import Data.List (nubBy, sort)
import qualified Data.Set as S
import qualified Data.MultiMap as MM

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

paths :: Int -> Int -> [(Int,Int)] -> [[Int]]
paths s t edges = search s t (S.singleton s) [s]
  where edgesMap = MM.fromList edges
        search x y vs path =
          let adjs' = MM.lookup x edgesMap
              adjs = filter (\u -> not $ u `S.member` vs) adjs'
              r = concatMap (\u -> search u y (u `S.insert` vs) (u:path)) adjs
           in if x == y
              then (reverse path: r)
              else r

cycle :: Int -> [(Int,Int)] -> [[Int]]
cycle s edges = search s S.empty [s]
  where edgesMap = MM.fromList edges
        search cur vs path =
          let adjs' = MM.lookup cur edgesMap
              adjs = S.fromList $ filter (\u -> not (u `S.member` vs) || u == s) adjs'
              ret = concatMap (\u -> search u (u `S.insert` vs) (u:path)) adjs
           in if s `S.member` adjs
              then (reverse (s:path) : ret)
              else ret

{-

expandEdges edges = concat $ map (\(u,v) -> if (v,u) `elem` edges then [(u,v)] else [(v,u),(u,v)]) edges

--assumed undirected graph
isConnected :: [Int] -> [(Int,Int)] -> Bool
isConnected ns edges = let edges' = expandEdges edges in
                         helper ns edges'
    where helper [] [] = True
          helper [] _ = True
          helper _ [] = False
          helper (n:ns) edges = and (map (\v -> (length (paths n v edges) > 0) ) ns) && isConnected ns edges

isTree :: [Int] -> [(Int,Int)] -> Bool
isTree ns edges = let n = length ns in
                    isConnected ns edges && (length edges) == (n-1)

spanTrees :: [Int] -> [(Int,Int)] -> [[(Int,Int)]]
spanTrees nodes edges
    | not (isConnected nodes edges) = []
    | isTree nodes edges = [edges]
    | otherwise = let le = length edges in 
                    foldr (++) [] $ map (\i -> let edges' = (take (i-1) edges) ++ (drop i edges) in
                                                 spanTrees nodes edges') [1..le]


primSpanTree :: Int -> [(Int,Int,Int)] -> ([(Int,Int)],Int)
primSpanTree n edges
    | not (isConnected [1..n] $ map (\(x,y,_) -> (x,y)) edges) = error "not connected"
    | otherwise = helper [1..n] ((1,0):[(i,-1) | i <- [2..n]]) 0 [] (constructMap [1..n] Map.empty)

    where weight (_,_,w) = w
          compareWeight e1 e2 = compare (weight e1) (weight e2)

          compareDis (i1,d1) (i2,d2) = case (d1,d2) of
                                        (-1,-1) -> EQ
                                        (-1,_) -> GT
                                        (_,-1) -> LT
                                        (_,_) -> compare d1 d2

          edge_map = em_helper Map.empty edges
            where em_helper m [] = m
                  em_helper m ((x,y,w):es) = em_helper (Map.insert (x,y) w (Map.insert (y,x) w m)) es

          relax mi (i,d) pre = case Map.lookup (mi,i) edge_map of
                                 Nothing -> ((i,d),pre)
                                 Just w -> if d == -1 || w < d then
                                                    ((i,w),Map.insert i mi pre)
                                                    else ((i,d),pre)

          constructMap [] r = r
          constructMap (n:ns) r = constructMap ns (Map.insert n (-1) r)

          updatePreAndDis _ [] pre = (pre,[])
          updatePreAndDis mi (p:ps) pre = let (p',pre') = relax mi p pre
                                              (pre'',ps') = updatePreAndDis mi ps pre' in
                                            (pre'',p':ps')

          helper _ [] s r _ = (r,s)
          helper ns mindis@(p:ps) s r pre = case p of 
                                                   (_,-1) -> error "not connected" --cannot happen
                                                   (mi,md) -> let (pre',ps') = updatePreAndDis mi ps pre
                                                                  r' = case Map.lookup mi pre' of
                                                                        Just (-1) -> r
                                                                        Just mipre -> ((mipre,mi):r)
                                                                        Nothing -> r
                                                                in
                                                                helper ns ps' (s+md) r' pre'

-}

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
      paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe`
        [[1,2,3,4],[1,3,4]]
      paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe`
        []

  describe "cycle" $ do
    it "works" $ do
      cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe`
        [[2,3,4,2]]
      cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe` []
