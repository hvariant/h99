import System.Random
import Data.List (sortBy)

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = (x:xs)
insertAt x (y:ys) n = (y:insertAt x ys (n-1))

range :: Int -> Int -> [Int]
range x y
    | x > y = []
    | x == y = [x]
    | x < y = (x:range (x+1) y)

removeAt :: Int -> [a] -> (a,[a])
removeAt 1 (x:xs) = (x,xs)
removeAt n (x:xs) = let (r,rs) = removeAt (n-1) (xs) in
                        (r,x:rs)

rnd_select :: [a] -> Int -> IO [a]
rnd_select _ 0 = return []
rnd_select xs n = do
    i <- getStdRandom (randomR (1,(length xs)))
    let (x,xs') = removeAt i xs
    xs'' <- rnd_select xs' (n-1)
    return (x:xs'')

diff_select :: Int -> Int -> IO [Int]
diff_select 0 m = return []
diff_select n m = do
    x <- getStdRandom (randomR (1,m))
    xs <- diff_select (n-1) m
    return (x:xs)

gen_permu :: [Int] -> IO [Int]
gen_permu [] = return []
gen_permu xs = do
    i <- getStdRandom (randomR (1,(length xs)))
    let (x',xs') = removeAt i xs
    xs'' <- gen_permu xs'
    return (x':xs'')

rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
    indexes <- gen_permu [0..(length xs)-1]
    return $ map (xs !!) indexes

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations 1 xs = map (\x -> [x]) xs
combinations k l@(x:xs) = if length l == k then
                            [l]
                            else (map (x:) (combinations (k-1) xs)) ++ (combinations k xs)

combinations' :: Int -> [a] -> [([a],[a])]
combinations' 0 _ = []
combinations' 1 xs = map f [1..length xs]
    where f i = let (y,ys) = removeAt i xs in
                ([y],ys)
combinations' k l@(x:xs) = if length l == k then
                            [(l,[])]
                            else (map (\(l1,l2) -> ((x:l1),l2) ) (combinations' (k-1) xs)) ++ (map (\(l1,l2) -> (l1,(x:l2)) ) (combinations' k xs))

group :: [Int] -> [a] -> [[[a]]]
group ns xs = if length ns == 1 then
                [[xs]]
                else let (n:ns') = ns
                        in foldr1 (++) $ map (\(x,r) -> map (x:) (group ns' r)) (combinations' n xs)

lsort :: [[a]] -> [[a]]
lsort = sortBy key
    where key l1 l2 = if length l1 < length l2 then
                        LT
                        else if length l1 > length l2 then
                                GT
                                else EQ

count :: (Eq a) => a -> [a] -> Int
count x [] = 0
count x (y:ys)
    | x == y = 1 + count x ys
    | otherwise = count x ys

lfsort :: [[a]] -> [[a]]
lfsort xs = sortBy key xs
    where freq l = count (length l) $ map length xs
          key l1 l2 = let f1 = freq l1
                          f2 = freq l2
                          in if f1 < f2 then
                                LT
                                else if f1 > f2 then
                                        GT
                                        else EQ

