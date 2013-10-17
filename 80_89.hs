--conversion
--omitted

paths :: Int -> Int -> [(Int,Int)] -> [[Int]]
paths s t edges = search s t [s] [s]
    where search x y vs path = let edges' = filter (\(u,_) -> u == x) edges
                                   adjs = map snd edges'
                                   adjs' = filter (\u -> not (u `elem` vs)) adjs
                                   ret = foldr (++) [] (map (\u -> search u y (u:vs) (u:path)) adjs') in
                                 if x == y then
                                    ((reverse path):ret)
                                    else ret



myCycle :: Int -> [(Int,Int)] -> [[Int]]
myCycle s edges = search s [] [s]
    where search cur vs path = let edges' = filter (\(u,_) -> u == cur) edges
                                   adjs = map snd edges'
                                   adjs' = filter (\u -> not (u `elem` vs) || u == s) adjs
                                   ret = foldr (++) [] (map (\u -> search u (u:vs) (u:path)) adjs') in
                                 if s `elem` adjs' then
                                    ((reverse (s:path)) : ret)
                                    else ret

