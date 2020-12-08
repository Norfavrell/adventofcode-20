import System.Environment
import Text.ParserCombinators.Parsec
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Arrow
import Debug.Trace
import Data.List

data Instr = Acc Int | Jmp Int | Nop Int deriving (Show)
data Result = Loop (Int, Int) | End Int deriving (Show, Eq)
type Program = Vector Instr

programParser :: Parser [Instr]
programParser = many1 anyInst <* eof
    where inst t s = t <$> (string s *> space *> num)
          anyInst  = (inst Acc "acc" <|> inst Jmp "jmp" <|> inst Nop "nop") <* optional newline

num :: Parser Int
num = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Int
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

step :: Vector Instr -> IntSet -> Int -> Int -> Result
step prog hist pc acc | IntSet.member pc hist = Loop (pc, acc)
                      | otherwise =
                          case inst of
                              Just (Acc v) -> step prog (IntSet.insert pc hist) (pc+1) (acc+v)
                              Just (Jmp i) -> step prog (IntSet.insert pc hist) (pc+i) acc
                              Just (Nop _) -> step prog (IntSet.insert pc hist) (pc+1) acc
                              Nothing      -> End acc
    where inst = (V.!?) prog pc

mutate :: [Instr] -> [Instr] -> [[Instr]]
mutate _ [] = []
mutate sf (x:xs)  = case x of
    Jmp v -> (sf ++ [Nop v] ++ xs) : mutate (sf ++ [x]) xs
    Nop v -> (sf ++ [Jmp v] ++ xs) : mutate (sf ++ [x]) xs
    _     -> mutate (sf ++ [x]) xs

solve prog = (solve1 prog, solve2 prog)

solve1 prog = step (V.fromList prog) IntSet.empty 0 0

solve2 :: [Instr] -> Maybe Result
solve2 = find isEnd . map solve1 . mutate []
    where isEnd (End _) = True 
          isEnd _  = False

main = getArgs >>= (readFile . head) >>=
    print . right solve . parse programParser ""