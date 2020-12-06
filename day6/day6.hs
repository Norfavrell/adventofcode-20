import System.Environment
import Data.List.Split
import Data.List

parseInput = map (splitOn "\n") . splitOn "\n\n"

solve f = sum . map (length . nub . f)
solve1 = solve concat
solve2 = solve $ foldr intersect ['a'..'z']

main = getArgs >>= (readFile . head) >>= 
    print . (\x -> (solve1 x, solve2 x)) . parseInput
