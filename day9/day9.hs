import System.Environment
import Data.List.Split
import Data.List
import Data.Functor

parse = map read . lines

window w [] = [w]
window w (x:xs) = w : window (tail w ++ [x]) xs

test v xs = or [(v-x) `elem` xs | x <- xs]

consets x = (drop 2 . inits) x ++ (consets . tail) x

solve1 w vals = (<&>) tested fst
    where tested     = find (not . uncurry test) candidates
          candidates = zip (drop w vals) $ uncurry window $ splitAt w vals

solve2 t = fmap solution . find ((==) t . sum) . consets
    where solution xs = (+) (minimum xs) (maximum xs)

solve w vals = (s1, s1 >>= (`solve2` vals))
    where s1 = solve1 w vals

main = getArgs >>= (readFile . head) >>=
    (print . solve 25 . parse)