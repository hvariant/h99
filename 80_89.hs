import Data.List (sortBy,find)
import qualified Data.Map as Map

--conversion
--omitted

paths :: Int -> Int -> [(Int,Int)] -> [[Int]]
paths s t edges = search s t [s] [s]
    where search x y vs path = let edges' = filter (\(u,_) -> u == x) edges
                                   adjs = map snd edges'
                                   adjs' = filter (\u -> not (u `elem` vs)) adjs
                                   ret = foldr (++) [] (map (\u -> search u y (u:vs) (u:path)) adjs') in
                                 if x == y then
                                    ((reverse path):ret)
                                    else ret



myCycle :: Int -> [(Int,Int)] -> [[Int]]
myCycle s edges = search s [] [s]
    where search cur vs path = let edges' = filter (\(u,_) -> u == cur) edges
                                   adjs = map snd edges'
                                   adjs' = filter (\u -> not (u `elem` vs) || u == s) adjs
                                   ret = foldr (++) [] (map (\u -> search u (u:vs) (u:path)) adjs') in
                                 if s `elem` adjs' then
                                    ((reverse (s:path)) : ret)
                                    else ret

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




-- Problem 85 to 89


