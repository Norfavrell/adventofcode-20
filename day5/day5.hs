import Data.Sort
import System.Environment

-- |TODO Clean this up

data Step = F | B | L | R deriving (Show, Eq)

parsePass :: String -> ([Step], [Step])
parsePass = splitAt 7 . map f
    where f 'F' = F
          f 'B' = B
          f 'L' = L
          f 'R' = R

biSearch :: (Int, Int) -> [Step] -> (Int, Int)
biSearch (l,h) [] = (l,h)
biSearch (l,h) (s:rest) =  biSearch (nl, nh) rest
    where isLo x = x `elem` [F,L]
          mid = fromIntegral (l+h)/2 :: Float
          nl = if isLo s then l else ceiling mid
          nh = if isLo s then floor mid else h

passID :: ([Step], [Step]) -> Int 
passID (v, h) = vr*8+hr
    where (vr,_) = biSearch (0,127) v
          (hr,_) = biSearch (0,7) h 

findMissing :: [Int] -> Int
findMissing (a:b:rest) | a + 2 == b = a + 1 
                       | otherwise  = findMissing(b:rest)

main = do
    args <- getArgs
    x <- readFile $ head args
    let passes = map parsePass $ lines x
    let ids = map passID passes 
    let missing = findMissing $ sort ids

    print $ maximum ids
    print missing