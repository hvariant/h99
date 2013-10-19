import Data.List

fullWords :: Int -> String
fullWords n = let sn = show n in
                foldr1 step $ map (\sx -> digits !! (read [sx])) (show n)
    where step s1 s2 = s1 ++ "-" ++ s2
          digits = ["zero","one","two","three","four","five","six","seven","eight","nine"]

identifier :: String -> Bool
identifier s = letters1 s
    where hyphen ('-':s) = letters1 s
          hyphen _ = False

          letters "" = True
          letters (s:ss)
            | (s `elem` ['a'..'z'] || s `elem` ['A'..'Z']) = letters ss
            | (s == '-') = hyphen (s:ss)
            | otherwise = False

          letters1 "" = False
          letters1 (s:ss)
            | (s `elem` ['a'..'z'] || s `elem` ['A'..'Z']) = letters ss
            | otherwise = False



