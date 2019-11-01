{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Data.List (group)
import Data.Maybe (isJust, catMaybes)

-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
-- https://wiki.haskell.org/Prime_numbers
import ONeillPrimes (primes, primesToLimit)

import Test.Hspec

isPrime :: Integer -> Bool
isPrime n = n > 1 &&
  foldr (\p r -> p*p > n || n `rem` p /= 0 && r) True primes

myGCD :: Integer -> Integer -> Integer
myGCD x 0 = (abs x)
myGCD 0 x = (abs x)
myGCD x y = myGCD y (x `mod` y)

coprime :: Integer -> Integer -> Bool
coprime x = (1 ==) . gcd x

totient :: Integer -> Integer
totient 1 = 1
totient n = fromIntegral . length . filter (coprime n) $ [1..(n-1)]

primeFactors :: Integer -> [Integer]
primeFactors n = reverse $ iter n primes []
    where iter _ [] _ = []
          iter 1 _ r = r
          iter n (k:ks) r
            | n `mod` k == 0 = iter (n `div` k) (k:ks) (k:r)
            | otherwise = iter n ks r

primeFactorMult :: Integer -> [(Integer, Int)]
primeFactorMult = map (\x -> (head x, length x)) . group . primeFactors

phi :: Integer -> Integer
phi n = product $ map (\(p,n) -> (p-1)*p^(n-1)) (primeFactorMult n)

primesR :: Integer -> Integer -> [Integer]
primesR a b = dropWhile (< a) $ primesToLimit b

goldbach :: Integer -> Maybe (Integer,Integer)
goldbach n = iter 2 (n-2)
    where iter x y
            | x > y = Nothing
            | (isPrime x) && (isPrime y) = Just (x,y)
            | otherwise = iter (x+1) (y-1)

goldbachList :: Integer -> Integer -> [(Integer,Integer)]
goldbachList a b = catMaybes . filter isJust . map goldbach . filter even $ [a..b]

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

goldbachList' :: Integer -> Integer -> Integer -> [(Integer,Integer)]
goldbachList' a b l = filter (both (> l)) . goldbachList a $ b

main :: IO ()
main = hspec $ do
  describe "isPrime" $ do
    it "should work" $ do
      isPrime 0 `shouldBe` False
      isPrime 1 `shouldBe` False
      isPrime 2 `shouldBe` True
      isPrime 7 `shouldBe` True
      isPrime 10 `shouldBe` False
      isPrime 29 `shouldBe` True
      isPrime (7919*7919) `shouldBe` False
      isPrime (-100) `shouldBe` False

  describe "myGCD" $ do
    it "should work" $ do
      myGCD 36 63 `shouldBe` 9
      myGCD (-3) (-6) `shouldBe` 3
      myGCD (-3) 6 `shouldBe` 3

  describe "coprime" $ do
    it "should work" $ do
      coprime 35 64 `shouldBe` True

  describe "totient" $ do
    it "should work" $ do
      totient 10 `shouldBe` 4
      totient 0 `shouldBe` 0
      totient (-12) `shouldBe` 0

  describe "primeFactors" $ do
    it "should work" $ do
      primeFactors 315 `shouldBe` [3, 3, 5, 7]

  describe "primeFactorMult" $ do
    it "should work" $ do
      primeFactorMult 315 `shouldBe` [(3,2), (5,1), (7,1)]

  describe "phi" $ do
    it "should work" $ do
      phi 10 `shouldBe` 4

  describe "primesR" $ do
    it "should work" $ do
      primesR 10 20 `shouldBe` [11, 13, 17, 19]

  describe "goldbach" $ do
    it "should work" $ do
      goldbach 28 `shouldBe` Just (5, 23)

  describe "goldbachList" $ do
    it "should work" $ do
      goldbachList 9 20 `shouldBe` [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]

  describe "goldbachList'" $ do
    it "should work" $ do
      goldbachList' 4 2000 50 `shouldBe` [(73,919),(61,1321),(67,1789),(61,1867)]
