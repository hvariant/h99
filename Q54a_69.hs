{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Q54a_69 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

import Test.Hspec

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- A tree of integers
tree4 :: Tree Int
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
treeStructEq (Branch _ t1 t2) (Branch _ t3 t4) = t1 `treeStructEq` t3 && t2 `treeStructEq` t4
treeStructEq _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ t1 t2) = t1 `treeStructEq` mirror t2

construct :: (Ord a) => [a] -> Tree a
construct = foldl insertTree Empty
  where insertTree Empty x = Branch x Empty Empty
        insertTree (Branch c t1 t2) x =
          case compare x c of
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
    minHeight n = ceiling . logBase 2 $ (fromIntegral (n+1) :: Double)

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
leaves (Branch _ t1 t2) = leaves t1 ++ leaves t2

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch c t1 t2) = (c:(internals t1)) ++ (internals t2)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch c _ _) 1 = [c]
atLevel (Branch _ t1 t2) h = atLevel t1 (h-1) ++ atLevel t2 (h-1)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = (Branch 'x' Empty Empty)
completeBinaryTree n =
  let h :: Integer
      h = (floor $ (logBase 2 . fromIntegral $ n :: Double)) + 1
      maxLeftLeaves = 2^(h - 2)
      deepestLeaves = n - 2^(h - 1) + 1
   in if deepestLeaves < maxLeftLeaves
      then let tr = 2^(h - 2) - 1
               tl = n - tr - 1
            in Branch 'x' (completeBinaryTree tl)
                          (completeBinaryTree tr)
      else let tl = 2^(h - 1) - 1
               tr = n - tl - 1
            in Branch 'x' (completeBinaryTree tl)
                          (completeBinaryTree tr)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ t1 t2) = 1 + countNodes t1 + countNodes t2

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t = t `treeStructEq` completeBinaryTree (countNodes t)

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c t1 t2) = [c] ++ "(" ++ (treeToString t1) ++ "," ++ (treeToString t2) ++ ")"

type Parser = Parsec Void String

stringToTree :: String -> Either (ParseErrorBundle String Void) (Tree Char)
stringToTree s = parse pTree "stringToTree" s
  where pTree = pBranch <|> pure Empty
        pBranch = letterChar >>= \c
               -> pSubTree c <|> pure (Branch c Empty Empty)
        pSubTree c = Branch c <$> pLeftTree <*> pRightTree
        pLeftTree = char '(' *> pTree <* char ','
        pRightTree = pTree <* char ')'

treeToPreorder :: Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder (Branch c t1 t2) = [c] ++ treeToPreorder t1 ++ treeToPreorder t2

treeToInorder :: Tree Char -> String
treeToInorder Empty = ""
treeToInorder (Branch c t1 t2) = treeToInorder t1 ++ [c] ++ treeToInorder t2

preInTree :: String -> String -> Tree Char
preInTree "" "" = Empty
preInTree po io =
  let root = head po
      (ioL, _:ioR) = break (== root) io
      l1 = length ioL
      poL = take l1 (tail po)
      poR = drop l1 (tail po)
   in Branch root (preInTree poL ioL) (preInTree poR ioR)

exampleTree :: Tree Char
exampleTree = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                                     (Branch 'e' Empty Empty))
                         (Branch 'c' Empty
                                     (Branch 'f' (Branch 'g' Empty Empty)
                                                 Empty))

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch c t1 t2) = [c] ++ (tree2ds t1) ++ (tree2ds t2)

ds2tree :: String -> Maybe (Tree Char)
ds2tree s = fst <$> iter s
    where iter ('.':xs) = Just (Empty,xs)
          iter (x:xs) = do
            (t1,r') <- iter xs
            (t2,r'') <- iter r'
            pure (Branch x t1 t2,r'')
          iter _ = Nothing

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
      (construct [3, 2, 5, 7, 1] :: Tree Int)
        `shouldBe` Branch 3 (Branch 2 (Branch 1 Empty
                                                Empty)
                                      Empty)
                            (Branch 5 Empty
                                      (Branch 7 Empty
                                                Empty))
    it "should produce symmetric trees" $ do
      (symmetric . construct $ ([5, 3, 18, 1, 4, 12, 21] :: [Int])) `shouldBe` True
      (symmetric . construct $ ([3, 2, 5, 7, 1] :: [Int])) `shouldBe` True

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
        Right (Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty)))

  describe "preorders|inorders" $ do
    it "works" $ do
      let Right t = stringToTree "a(b(d,e),c(,f(g,)))"
          po = treeToPreorder t
          io = treeToInorder t
       in preInTree po io `shouldBe` t

  describe "ds2tree|tree2ds" $ do
    it "works" $ do
      ds2tree (tree2ds exampleTree) `shouldBe` Just exampleTree
