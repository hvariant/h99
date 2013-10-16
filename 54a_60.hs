data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf x = Branch x Empty Empty

tree1' = Branch 'a' (Branch 'b' (leaf 'd')
                                (leaf 'e'))
                    (Branch 'c' Empty
                                (Branch 'f' (leaf 'g')
                                            Empty))

-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty
 
-- An empty binary tree
tree3 = Empty
 
-- A tree of integers
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)


reduce c ts1 ts2 = map f [(t1,t2) | t1 <- ts1, t2 <- ts2]
    where f (t1,t2) = Branch c t1 t2

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n 
    | (n-1) `mod` 2 == 0 = let sub = (cbalTree ((n-1) `div` 2)) in
                            reduce 'x' sub sub
    | otherwise = let x = (n-1) `div` 2 in
                    (reduce 'x' (cbalTree x) (cbalTree (n-1-x))) ++ (reduce 'x' (cbalTree (n-1-x)) (cbalTree x))


mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Branch c t1 t2) = Branch c (mirror t2) (mirror t1)

treeStructEq :: Tree a -> Tree b -> Bool
treeStructEq Empty Empty = True
treeStructEq (Branch _ t1 t2) (Branch _ t3 t4) = treeStructEq t1 t3 && treeStructEq t2 t4
treeStructEq _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ t1 t2) = t1 `treeStructEq` mirror t2

construct :: (Ord a) => [a] -> Tree a
construct xs = foldl insertTree Empty xs
    where insertTree Empty x = Branch x Empty Empty
          insertTree (Branch c t1 t2) x = case compare x c of
                                                LT -> (Branch c (insertTree t1 x) t2)
                                                EQ -> (Branch c (insertTree t1 x) t2)
                                                GT -> (Branch c t1 (insertTree t2 x))


symCbalTrees :: Int -> [Tree Char]
symCbalTrees 0 = []
symCbalTrees n
    | (n-1) `mod` 2 == 0 = let sub = (cbalTree ((n-1) `div` 2)) in
                            [(Branch 'x' (mirror t) t) | t <- sub]
    | otherwise = []

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree c h
    | h == 1 = [Branch c Empty Empty]
    | h >= 2 = let ts1 = hbalTree c (h-1)
                   ts2 = hbalTree c (h-2) in
                 [Branch c t1 t2 | t1 <- ts1, t2 <- ts1] ++ 
                 [Branch c t1 t2 | t1 <- ts1, t2 <- ts2] ++ 
                 [Branch c t1 t2 | t1 <- ts2, t2 <- ts1]

minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes 2 = 2
minNodes h = minNodes (h-1) + minNodes (h-2)

maxNodes :: Int -> Int
maxNodes h = 2^h - 1

maxHeight :: Int -> Int
maxHeight n = last $ takeWhile (\h -> (minNodes h) <= n) [1..]

minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 (fromIntegral (n+1))

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes c 1 = [Branch c Empty Empty]
hbalTreeNodes c n = let minH = minHeight n
                        maxH = maxHeight n in
                      foldr (++) [] $ map (hbal_nh n) [minH..maxH]
    where gen_tree h1 h2 n ns1 ns2 = foldr (++) [] [[Branch c t1 t2 | t1 <- hbal_nh n1 h1 , t2 <- hbal_nh (n-n1-1) h2] | n1 <- ns1 , (n-n1-1) `elem` ns2]

          hbal_nh _ 0 = [Empty]
          hbal_nh _ 1 = [Branch c Empty Empty]
          hbal_nh n h = let minN1 = minNodes (h-1)
                            maxN1 = maxNodes (h-1)
                            minN2 = minNodes (h-2)
                            maxN2 = maxNodes (h-2) in
                          (gen_tree (h-1) (h-1) n [minN1..maxN1] [minN1..maxN1]) ++ 
                          (gen_tree (h-1) (h-2) n [minN1..maxN1] [minN2..maxN2]) ++ 
                          (gen_tree (h-2) (h-1) n [minN2..maxN2] [minN1..maxN1])

