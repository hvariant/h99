import qualified Data.Map as Map
import qualified Data.Set as Set

queens :: Int -> [[Int]]
queens 0 = []
queens 1 = [[1]]
queens n = iter 1 Set.empty Set.empty Set.empty []
    where check x y vrow vv1 vv2 = not (Set.member y vrow ||
                                        Set.member (x-y) vv1 ||
                                        Set.member (x+y) vv2)

          iter x vrow vv1 vv2 cur
            | x == (n+1) = [cur]
            | otherwise = let ys = filter (\y -> check x y vrow vv1 vv2) [1..n] in
                            concat $ map (\y -> let vrow' = Set.insert y vrow
                                                    vv1' = Set.insert (x-y) vv1
                                                    vv2' = Set.insert (x+y) vv2 in
                                                  iter (x+1) vrow' vv1' vv2' (cur++[y])) ys



knightsTo :: Int -> (Int,Int) -> [[(Int,Int)]]
knightsTo n (x0,y0) = iter (x0,y0) [(x0,y0)] $ Set.singleton (x0,y0)
    where deltas = [(1,2),(1,-2),(-1,2),(-1,-2),
                    (2,1),(2,-1),(-2,1),(-2,-1)]

          iter (cx,cy) path vs
            | (Set.size vs) == (n*n) = [reverse path]
            | otherwise = let next = map (\(dx,dy) -> (cx+dx,cy+dy)) deltas
                              next' = filter (\(nx,ny) ->
                                                    (nx > 0) && (ny > 0) && (nx <= n) && (ny <= n) && not (Set.member (nx,ny) vs)) next in
                            concat $ map (\(nx,ny) -> iter (nx,ny) ((nx,ny):path) (Set.insert (nx,ny) vs)) next'

