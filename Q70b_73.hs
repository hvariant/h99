{-# OPTIONS_GHC -Wall #-}

module Q70b_73 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

import Test.Hspec

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

tree1 :: Tree Char
tree1 = Node 'a' []

tree2 :: Tree Char
tree2 = Node 'a' [Node 'b' []]

tree3 :: Tree Char
tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 :: Tree Char
tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 :: Tree Char
tree5 = Node 'a' [Node 'f' [Node 'g' []],
                  Node 'c' [],
                  Node 'b' [Node 'd' [], Node 'e' []]
                 ]

nnodes :: Tree a -> Int
nnodes (Node _ l) = 1 + sum (fmap nnodes l)

treeToString :: Tree Char -> String
treeToString (Node c sub) = c : (concatMap treeToString sub) ++ "^"

type Parser = Parsec Void String

stringToTree :: String -> Tree Char
stringToTree s = case parse pTree "parse error" s of
                    Left _ -> error "parse error"
                    Right t -> t
  where pTree :: Parser (Tree Char)
        pTree = letterChar >>= \c
             -> Node c <$> (many pTree <* char '^')

ipl :: Tree a -> Int
ipl (Node _ []) = 0
ipl (Node _ sub) = let r = map ipl sub in
                     sum $ map (\x -> (2*x+1)) r

bottomUp :: Tree Char -> String
bottomUp (Node c []) = [c]
bottomUp (Node c sub) = (concatMap bottomUp sub) ++ [c]

lispDisp :: Tree Char -> String
lispDisp (Node c []) = [c]
lispDisp (Node c sub) = "(" ++ [c] ++ " " ++ (foldr step "" $ fmap lispDisp sub) ++ ")"
    where step s1 "" = s1
          step s1 s2 = s1 ++ " " ++ s2

main :: IO ()
main = hspec $ do
  describe "stringToTree | treeToString" $ do
    it "works" $ do
      treeToString (stringToTree "afg^^c^bd^e^^^") `shouldBe` "afg^^c^bd^e^^^"

  describe "ipl" $ do
    it "works" $ do
      ipl tree5 `shouldBe` 9
      ipl tree4 `shouldBe` 2

  describe "bottomUp" $ do
    it "works" $ do
      bottomUp tree5 `shouldBe` "gfcdeba"

  describe "lispDisp" $ do
    it "works" $ do
      lispDisp tree1 `shouldBe` "a"
      lispDisp tree2 `shouldBe` "(a b)"
      lispDisp tree3 `shouldBe` "(a (b c))"
      lispDisp tree4 `shouldBe` "(b d e)"
      lispDisp tree5 `shouldBe` "(a (f g) c (b d e))"
