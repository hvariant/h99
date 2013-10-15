myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (x:xs) = myButLast xs

elementAt (x:_) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

myLength [] = 0
myLength xs = iter xs 0
    where iter [] r = r
          iter (x:xs) r = iter xs (r+1)

myReverse [] = []
myReverse xs = iter xs []
    where iter [] r = r
          iter (x:xs) r = iter xs (x:r)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (myReverse xs)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = foldr step [] xs
    where step x r = (flatten x) ++ r

compress :: (Eq a) => [a] -> [a]
compress xs = reverse $ foldl step [] xs
    where step [] x = [x]
          step r@(y:ys) x = if y == x then r else (x:r)
                        
pack :: (Eq a) => [a] -> [[a]]
pack xs = reverse $ foldl step [] xs
    where step [] x = [[x]]
          step (y:ys) x = if head y == x then ((x:y):ys) else ([x]:y:ys)

encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = reverse $ foldl step [] xs
    where step [] x = [(1,x)]
          step ((ny,y):ys) x = if y == x then ((ny+1,y):ys) else ((1,x):(ny,y):ys)

