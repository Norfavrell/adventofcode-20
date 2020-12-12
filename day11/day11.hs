import System.Environment
import Data.Matrix hiding (trace)
import Data.Maybe

data Cell = Empty | Occupied | Floor deriving (Show, Eq)

-- parse :: String -> Matrix Cell
parse = fromLists . map (map cell) . lines
    where cell 'L' = Empty
          cell '#' = Occupied
          cell '.' = Floor

adjacent matrix i j = mapMaybe get ([(i+x,j+y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0])
    where get p = uncurry safeGet p matrix


adjacent' :: Matrix Cell -> Int -> Int -> [Cell]
adjacent' m i j = map see [n,e,s,w,ne,se,sw,nw]
    where n  = [m' (i-x,j) | x <- takeWhile (\y -> i-y>0) [1..]]
          e  = [m' (i,j+x) | x <-takeWhile (\y -> j+y<=ncols m) [1..]]
          s  = [m' (i+x,j) | x <- takeWhile (\y -> i+y<=nrows m) [1..]]
          w  = [m' (i,j-x) | x <- takeWhile (\y -> j-y>0) [1..]]
          ne = [m' (i-x, j+x) | x <- takeWhile (\y -> i-y>0 && j+y<=ncols m) [1..]]
          se = [m' (i+x, j+x) | x <- takeWhile (\y -> i+y<=nrows m && j+y<=ncols m) [1..]]
          sw = [m' (i+x, j-x) | x <- takeWhile (\y -> i+y<=nrows m && j-y>0) [1..]]
          nw = [m' (i-x, j-x) | x <- takeWhile (\y -> i-y>0 && j-y>0) [1..]]
          m' = (!) m

see :: [Cell] -> Cell
see (Occupied:_) = Occupied
see (Empty:_) = Empty
see (Floor:xs) = see xs
see [] = Empty


mutate :: Int -> Cell -> [Cell] -> Cell
mutate _ Floor _ = Floor
mutate _ Empty nei | Occupied `elem` nei = Empty
                   | otherwise = Occupied
mutate t Occupied nei | length (filter (==Occupied) nei) >= t = Empty
                      | otherwise = Occupied

step :: (Cell -> [Cell] -> Cell) -> (Matrix Cell -> Int -> Int -> [Cell]) -> Matrix Cell -> Matrix Cell
step mut adj x = mapPos stepCell x
    where stepCell (i,j) v = mut v (adj x i j)

-- evolve :: Int -> (Matrix Cell -> Int -> Int -> [Cell]) ->  Matrix Cell -> [Cell]
evolve t adj = f . iterate (step (mutate t) adj)
    where f (x:y:ys) | x == y = [x]
                     | otherwise = x : f (y:ys)

-- solve1 :: Matrix Cell -> Int 
solve1 m = length . filter (==Occupied) . toList $ last (evolve 4 adjacent m)

solve2 m = length . filter (==Occupied) . toList $ last (evolve 5 adjacent' m)

solve m = (solve1 m, solve2 m)

main = getArgs >>= (readFile . head) >>=
    (print . solve . parse)
