import Text.ParserCombinators.Parsec
import System.Environment
import qualified Data.HashMap as M
import Control.Arrow
import Data.Maybe
import Data.List

-- | TODO this is a good problem to practice tying the knot 

inputParser = many1 (line <* optional newline) <* eof
    where line = (,) <$> bag <* string " contain "
                     <*> ((many1 (bagn <* optional (string ", ")) <* string ".")
                     <|> ([] <$ string "no other bags."))
          bag = (++) <$> many1 alphaNum <* space <*> many1 alphaNum <* string " bag" <* optional (string "s")
          bagn = (,) <$> (read <$> many1 digit) <* space <*> bag

edges = concatMap f
    where f (v,(w,t):rest) = (v, t, w):f (v, rest)
          f (_, []) = []

invertEdges = map (\(s,t,w) -> (t,s,w))

asGraph = foldr f M.empty
    where f (s,t,w) = M.insertWith (++) s [(t,w)]

travel g v l@(n,_) | n `elem` v      = []
                   | null children   = [l]
                   | otherwise       = l:concatMap travel' children
    where children = concat $ maybeToList (M.lookup n g)
          travel' = travel g (n:v)

travel2 graph visited (node,weight) | node `elem` visited = []
                                    | null children = [weight]
                                    | otherwise = weight:map (weight *) (concatMap (travel2 graph (node:visited)) children)
    where children = concat $ maybeToList (M.lookup node graph)

solve1 g = (length .  nub . map fst $ p) - 1
    where p = travel g [] ("shinygold", 0)

solve2 g = sum (travel2 g [] ("shinygold", 1)) - 1

solve i = (solve1 g, solve2 g')
    where g  = asGraph . invertEdges . edges $ i
          g' = asGraph . edges $ i

main = getArgs >>= (readFile . head) >>=
    print . right solve . parse inputParser ""
