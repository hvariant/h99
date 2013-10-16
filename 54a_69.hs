import Text.ParserCombinators.Parsec

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


countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ t1 t2) = countLeaves t1 + countLeaves t2

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch c Empty Empty) = [c]
leaves (Branch c t1 t2) = leaves t1 ++ leaves t2

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch c t1 t2) = (c:(internals t1)) ++ (internals t2)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch c _ _) 1 = [c]
atLevel (Branch c t1 t2) h = atLevel t1 (h-1) ++ atLevel t2 (h-1)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = (Branch 'x' Empty Empty)
completeBinaryTree n = let h = (floor $ logBase 2 (fromIntegral (n+1))) - 1
                           res = n - (2^(h+1)-1) in
                         if res <= 2^h then
                            Branch 'x' (completeBinaryTree ((n-1-res) `div` 2 + res)) (completeBinaryTree ((n-1-res) `div` 2))
                            else Branch 'x' (completeBinaryTree (2^(h+1)-1)) (completeBinaryTree (((n-1-res) `div` 2) + (res - 2^h)))
                         

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ t1 t2) = 1 + countNodes t1 + countNodes t2

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t = t `treeStructEq` completeBinaryTree (countNodes t)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )


layout :: Tree a -> Tree (a,(Int,Int))
layout Empty = Empty
layout t = iter t 1 1 (countNodes t)
    where iter Empty _ _ _ = Empty
          iter (Branch c t1 t2) h minX maxX = let c1 = countNodes t1 in
                                              (Branch (c,(minX + c1,h)) (iter t1 (h+1) minX (minX + c1 - 1))
                                                                        (iter t2 (h+1) (minX + c1 + 1) maxX))

height :: Tree a -> Int
height Empty = 0
height (Branch _ t1 t2) = 1 + max (height t1) (height t2)


tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )


minOffset Empty = 0
minOffset (Branch (_,(o,_)) t1 t2) = minimum [o,(minOffset t1),(minOffset t2)]

maxOffset Empty = 0
maxOffset (Branch (_,(o,_)) t1 t2) = maximum [o,(maxOffset t1),(maxOffset t2)]

offset x Empty = Empty
offset x (Branch (c,(o,h)) t1 t2) = Branch (c,(o-x,h)) (offset x t1) (offset x t2)

layout' :: Tree a -> Tree (a,(Int,Int))
layout' t = let h = height t
                width = if h < 2 then 0 else 2^(h-2)
                t' = iter t 0 1 width
                minO = minOffset t' in
              offset (minO - 1) t'
    where iter Empty _ _ _ = Empty
          iter (Branch c t1 t2) o h w = Branch (c,(o,h)) (iter t1 (o-w) (h+1) (w `div` 2))
                                                         (iter t2 (o+w) (h+1) (w `div` 2))


emptyLeft :: Tree a -> Bool
emptyLeft Empty = True
emptyLeft (Branch _ t1 _) = t1 `treeStructEq` Empty

emptyRight :: Tree a -> Bool
emptyRight Empty = True
emptyRight (Branch _ _ t2) = t2 `treeStructEq` Empty

layout'' :: Tree a -> Tree (a,(Int,Int))
layout'' t = let t' = iter t 0 1
                 minO = minOffset t' in
               offset (minO - 1) t'
    where iter Empty _ _ = Empty
          iter (Branch c Empty t2) o h = Branch (c,(o,h)) Empty (iter t2 (o+1) (h+1))
          iter (Branch c t1 Empty) o h = Branch (c,(o,h)) (iter t1 (o-1) (h+1)) Empty
          iter (Branch c t1 t2) o h = if not (emptyLeft t2) && not (emptyRight t1) then
                                        Branch (c,(o,h)) (iter t1 (o-2) (h+1)) (iter t2 (o+2) (h+1))
                                        else Branch (c,(o,h)) (iter t1 (o-1) (h+1)) (iter t2 (o+1) (h+1))



treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c t1 t2) = [c] ++ "(" ++ (treeToString t1) ++ "," ++ (treeToString t2) ++ ")"

stringToTreeRules :: Parser (Tree Char)
stringToTreeRules = do
    mx <- optionMaybe (oneOf ['a'..'z'])
    case mx of
        Nothing -> 
                    return Empty
        Just x -> do 
                      m <- optionMaybe (char '(')
                      (t1,t2) <- case m of
                                    Just _ -> do 
                                                t1 <- stringToTreeRules
                                                char ','
                                                t2 <- stringToTreeRules
                                                char ')'
                                                return (t1,t2)
                                    Nothing -> return (Empty,Empty)

                      return (Branch x t1 t2)

stringToTree :: String -> Tree Char
stringToTree s = case (parse stringToTreeRules "parse error" s) of
                Left _ -> error "parse error"
                Right t -> t


treeToPreorder :: Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder (Branch c t1 t2) = [c] ++ treeToPreorder t1 ++ treeToPreorder t2

treeToInorder :: Tree Char -> String
treeToInorder Empty = ""
treeToInorder (Branch c t1 t2) = treeToInorder t1 ++ [c] ++ treeToInorder t2

preInTree :: String -> String -> Tree Char
preInTree "" "" = Empty
preInTree po io = let (io1,io2') = break (==(head po)) io
                      io2 = tail io2'
                      l1 = length io1
                      po' = tail po in 
                      Branch (head po) (preInTree (take l1 po') io1) (preInTree (drop l1 po') io2)


example = (Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty)))

tree2ds :: Tree Char -> [Char]
tree2ds Empty = "."
tree2ds (Branch c t1 t2) = [c] ++ (tree2ds t1) ++ (tree2ds t2)


ds2tree :: String -> Tree Char
ds2tree s = fst $ iter s
    where iter ('.':xs) = (Empty,xs)
          iter (x:xs) = let (t1,r') = iter xs
                            (t2,r'') = iter r' in
                          (Branch x t1 t2,r'')

