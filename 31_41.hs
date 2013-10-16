isPrime :: Int -> Bool
isPrime n = let l = floor (sqrt (fromIntegral n)) in
                and $ map (\x -> n `mod` x /= 0) [2..l]

myGCD :: Int -> Int -> Int
myGCD x 0 = (abs x)
myGCD 0 x = (abs x)
myGCD x y = myGCD y (x `mod` y)

coprime :: Int -> Int -> Bool
coprime x y = 1 == gcd x y

totient :: Int -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..(n-1)]

primeFactors :: Int -> [Int]
primeFactors n = reverse $ iter n 2 []
    where iter 1 k r = r
          iter n k r
            | n `mod` k == 0 = iter (n `div` k) k (k:r)
            | otherwise = iter n (k+1) r

encode :: (Eq a) => [a] -> [(a,Int)]
encode xs = foldr step [] xs
    where step x [] = [(x,1)]
          step x ((y,ny):ys) = if y == x then ((y,ny+1):ys) else ((x,1):(y,ny):ys)

primeFactorMult :: Int -> [(Int,Int)]
primeFactorMult = encode . primeFactors

phi :: Int -> Int
phi n = product $ map (\(p,n) -> (p-1)*p^(n-1)) (primeFactorMult n)

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

goldbach :: Int -> (Int,Int)
goldbach n = iter 2 (n-2)
    where iter x y
            | x > y = error "cannot find pair"
            | otherwise = if (isPrime x) && (isPrime y) then
                             (x,y)
                             else iter (x+1) (y-1)

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList a b = map goldbach $ filter even [a..b]

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' a b l = filter (\(x,y) -> x > l && y > l) $ map goldbach $ filter even [a..b]


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

