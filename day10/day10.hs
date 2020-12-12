import System.Environment
import Data.List
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V
import Data.MemoTrie

parse :: String -> [Int]
parse = map read . lines


countPaths :: Vector Int -> Int
countPaths vals = countPaths'' 0
    where isReachable c x = maybe False (\y -> (c-y)<4) $ vals !? x
          countPaths'' = memo countPaths'
          countPaths' x | x + 1 == length vals = 1 
                        | otherwise = sum $ map countPaths'' $ filter (isReachable (vals ! x)) [x+1..x+3]

tadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tadd (a,b) (c,d) = (a+c, b+d)

solve1' :: [Int] -> (Int, Int)
solve1' [_] = (1,1)
solve1' (x:y:xs) | (y-x) == 1 = tadd (1,0) (solve1' (y:xs))
                 | (y-x) == 3 = tadd (0,1) (solve1' (y:xs))

solve1 :: [Int] -> Int
solve1 = uncurry (*) . solve1'

solve2 :: [Int] -> Int
solve2 xs = countPaths (V.fromList $ reverse $ sort (0:max:xs))
    where max = maximum xs + 3

solve xs = (solve1 vals, solve2 vals)
    where vals = sort xs

main = getArgs >>= (readFile . head) >>=
    (print . solve . sort . parse)
