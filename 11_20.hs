data Cell a = Multiple Int a | Single a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Cell a]
encodeModified xs = foldr step [] xs
    where step x [] = [Single x]
          step x ((Single y):cys) = if y == x then
                                    ((Multiple 2 x):cys)
                                  else ((Single x):(Single y):cys)
          step x ((Multiple ny y):cys) = if y == x then
                                        ((Multiple (ny+1) x):cys)
                                  else ((Single x):(Multiple ny y):cys)

decodeModified :: [Cell a] -> [a]
decodeModified xs = foldl step [] xs
    where unfold (Single x) = [x]
          unfold (Multiple n x) = take n $ repeat x
          step r x = r ++ unfold x

dupli :: [a] -> [a]
dupli = foldr1 (++) . map (\x -> [x,x])

repli :: [a] -> Int -> [a]
repli xs n = foldr1 (++) $ map (\x -> replicate n x) xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = if length xs < n then
                    xs
                    else (take (n-1) xs) ++ (dropEvery (drop n xs) n)

split :: [a] -> Int -> ([a],[a])
split xs n = iter xs [] n
    where 
        iter right left 0 = (left,right)
        iter right left n = iter (tail right) (left ++ [head right]) (n-1)

slice :: [a] -> Int -> Int -> [a]
slice xs i j = iter xs i j
    where
        iter xs 1 j = take j xs
        iter (x:xs) i j = iter xs (i-1) (j-1)

rotate :: [a] -> Int -> [a]
rotate xs n = if n > length xs
                then rotate xs (n `mod` (length xs))
                else if n < 0 then
                        reverse $ rotate (reverse xs) (-n)
                        else (drop n xs) ++ (take n xs)

removeAt :: Int -> [a] -> (a,[a])
removeAt 1 (x:xs) = (x,xs)
removeAt n (x:xs) = let (r,rs) = removeAt (n-1) (xs) in
                        (r,x:rs)
