import Data.List

and' :: Bool -> Bool -> Bool
or' :: Bool -> Bool -> Bool
nand' :: Bool -> Bool -> Bool
nor' :: Bool -> Bool -> Bool
xor' :: Bool -> Bool -> Bool
impl' :: Bool -> Bool -> Bool
equ' :: Bool -> Bool -> Bool

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'` -- was 7, changing it to 3 got me the same results as in the original question :(

and' x y = x && y
or' x y = x || y
nand' x y = not (and' x y)
nor' x y = not (or' x y)
xor' x y = x /= y
impl' x y = (not x) `or'` y
equ' x y = x == y

gen_entry :: Int -> [[Bool]]
gen_entry 0 = []
gen_entry 1 = [[True],[False]]
gen_entry k = let rs' = gen_entry (k-1) in
                (map (True:) rs') ++ (map (False:) rs')

pairs :: [[Bool]]
pairs = gen_entry 2

table2 :: (Bool -> Bool -> Bool) -> String
table2 bf = unlines $ map f pairs
    where f (x:y:[]) = show x ++ " " ++ show y ++ " " ++ show (bf x y)

tablen :: Int -> ([Bool] -> Bool) -> String
tablen n nf = unlines $ map f (gen_entry n)
    where f np = (foldr1 (\x -> (\r -> x ++ " " ++ r)) (map show np)) ++ " " ++  (show (nf np))

gray :: Int -> [String]
gray 0 = []
gray 1 = ["0","1"]
gray n = let r' = gray (n-1) in
            (map ('0':) r') ++ (map ('1':) (reverse r'))

{-huffman :: (Eq a) => [(a,Int)] -> [(a,String)]-}
data Tree a = Leaf Int a | Node Int (Tree a) (Tree a) deriving (Show)

treeFreq (Leaf n _) = n
treeFreq (Node n _ _) = n

treeComp t1 t2 = compare (treeFreq t1) (treeFreq t2)


insertSorted :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertSorted _ x [] = [x]
insertSorted f x l@(y:ys) = case f x y of
                                EQ -> (x:l)
                                LT -> (x:l)
                                GT -> (y:(insertSorted f x ys))

huffman xs = let xs' = sortBy (\(_,f1) -> (\(_,f2) -> compare f1 f2)) xs
                 tree = buildtree (map (\(x,n) -> Leaf n x) xs') in
                 reverse $ traverse tree [] ""
    where buildtree (x:[]) = x
          buildtree (x1:x2:xs) = let f1 = treeFreq x1
                                     f2 = treeFreq x2 in
                                     buildtree $ insertSorted treeComp (Node (f1+f2) x1 x2) xs

          traverse (Leaf _ x) codes path = ((x,path):codes)
          traverse (Node _ t1 t2) codes path = let c1 = traverse t1 codes (path ++ "0") in
                                                traverse t2 c1 (path ++ "1")

