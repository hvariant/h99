
import Data.Bool (bool)
import Text.ParserCombinators.Parsec

import Test.Hspec

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

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n
  | (n-1) `mod` 2 == 0 = let sub = (cbalTree ((n-1) `div` 2))
                          in (Branch 'x' <$> sub <*> sub)
  | otherwise = let x = (n-1) `div` 2
                    t1 = cbalTree x
                    t2 = cbalTree (n-1-x)
                 in (Branch 'x' <$> t1 <*> t2)
                 ++ (Branch 'x' <$> t2 <*> t1)

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
construct = foldl insertTree Empty
  where insertTree Empty x = Branch x Empty Empty
        insertTree (Branch c t1 t2) x = case compare x c of
                                          LT -> (Branch c (insertTree t1 x) t2)
                                          EQ -> (Branch c (insertTree t1 x) t2)
                                          GT -> (Branch c t1 (insertTree t2 x))

symCbalTrees :: Int -> [Tree Char]
symCbalTrees 0 = []
symCbalTrees n
    | (n-1) `mod` 2 == 0 = let sub = cbalTree $ (n-1) `div` 2
                            in zipWith (Branch 'x') (mirror <$> sub) sub
    | otherwise = []

hbalTree :: a -> Int -> [Tree a]
hbalTree c = map fst . hbalTree'
  where hbalTree' 0 = [(Empty, 0)]
        hbalTree' 1 = [(Branch c Empty Empty, 1)]
        hbalTree' n
          | n < 0 = []
          | otherwise = let t = hbalTree' (n-1) ++ hbalTree' (n-2)
                            f (t1, h1) (t2, h2) = (,) (Branch c t1 t2) (1 + max h1 h2)
                         in filter (\(_, h) -> h == n) (f <$> t <*> t)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes c 1 = [Branch c Empty Empty]
hbalTreeNodes c n
  | n < 0 = []
  | otherwise =
    let minH = minHeight n
        maxH = maxHeight n
     in concatMap (\h -> hbalGen c h n) [minH..maxH]
  where
    minNodes :: Int -> Int
    minNodes 0 = 0
    minNodes 1 = 1
    minNodes 2 = 2
    minNodes h = 1 + (minNodes (h-1)) + (minNodes (h-2))

    maxNodes :: Int -> Int
    maxNodes h = 2^h - 1

    maxHeight :: Int -> Int
    maxHeight n = last $ takeWhile (\h -> minNodes h <= n) [0..]

    minHeight :: Int -> Int
    minHeight n = ceiling . logBase 2 $ fromIntegral (n+1)

    hbalGen :: a -> Int -> Int -> [Tree a]
    hbalGen _ 0 _ = [Empty]
    hbalGen c 1 _ = [Branch c Empty Empty]
    hbalGen c h n = do
      (h1, h2) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)]
      let minN1 = max (minNodes h1) (n - 1 - maxNodes h2)
      let maxN1 = min (maxNodes h1) (n - 1 - minNodes h2)
      n1 <- [minN1..maxN1]
      let n2 = n - 1 - n1
      Branch c <$> (hbalGen c h1 n1) <*> (hbalGen c h2 n2)

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
                      po' = tail po
                   in Branch (head po) (preInTree (take l1 po') io1) (preInTree (drop l1 po') io2)


exampleTree = (Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty)))

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch c t1 t2) = [c] ++ (tree2ds t1) ++ (tree2ds t2)

ds2tree :: String -> Tree Char
ds2tree s = fst $ iter s
    where iter ('.':xs) = (Empty,xs)
          iter (x:xs) = let (t1,r') = iter xs
                            (t2,r'') = iter r'
                         in (Branch x t1 t2,r'')

main :: IO ()
main = hspec $ do
  describe "cbalTree" $ do
    it "works" $ do
      length (cbalTree 4) `shouldBe` 4

  describe "symmetric" $ do
    it "works" $ do
      symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
        `shouldBe` False
      symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
        `shouldBe` True

  describe "construct" $ do
    it "works" $ do
      construct [3, 2, 5, 7, 1]
        `shouldBe` Branch 3 (Branch 2 (Branch 1 Empty
                                                Empty)
                                      Empty)
                            (Branch 5 Empty
                                      (Branch 7 Empty
                                                Empty))
    it "should produce symmetric trees" $ do
      (symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]) `shouldBe` True
      (symmetric . construct $ [3, 2, 5, 7, 1]) `shouldBe` True

  describe "symCbalTrees" $ do
    it "works" $ do
      length (symCbalTrees 5) `shouldBe` 2

  describe "hbalTree" $ do
    it "works" $ do
      (length $ hbalTree 'x' 0) `shouldBe` 1
      (length $ hbalTree 'x' 1) `shouldBe` 1
      (length $ hbalTree 'x' 2) `shouldBe` 3
      (length $ hbalTree 'x' 3) `shouldBe` 15

  describe "hbalTreeNodes" $ do
    it "works" $ do
      length (hbalTreeNodes 'x' 15) `shouldBe` 1553

  describe "countLeaves" $ do
    it "works" $ do
      countLeaves tree4 `shouldBe` 2

  describe "leaves" $ do
    it "works" $ do
      leaves tree4 `shouldBe` [4, 2]

  describe "internals" $ do
    it "works" $ do
      internals tree4 `shouldBe` [1, 2]

  describe "atLevel" $ do
    it "works" $ do
      atLevel tree4 2 `shouldBe` [2, 2]

  describe "completeBinaryTree" $ do
    it "works" $ do
      completeBinaryTree 4 `shouldBe`
        Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)

  describe "isCompleteBinaryTree" $ do
    it "works" $ do
      isCompleteBinaryTree (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
        `shouldBe` True

  describe "stringToTree" $ do
    it "works" $ do
      stringToTree "x(y,a(,b))" `shouldBe`
        Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

  describe "preorders|inorders" $ do
    it "works" $ do
      let t = stringToTree "a(b(d,e),c(,f(g,)))"
          po = treeToPreorder t
          io = treeToInorder t
       in preInTree po io `shouldBe` t

  describe "ds2tree|tree2ds" $ do
    it "works" $ do
      ds2tree (tree2ds exampleTree) `shouldBe` exampleTree
