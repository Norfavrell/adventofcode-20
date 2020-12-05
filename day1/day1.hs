import System.Environment

-- |For a given (number of entries, target, values) return list of target numbers
findNumbers :: (Ord a, Num a) => a -> a -> [a] -> [a]
findNumbers _ _ []                    = []
findNumbers 0 _ _                     = []
findNumbers 1 t (e:rest) | e == t     = [e]
                         | otherwise  = findNumbers 1 t rest
findNumbers n t (e:rest) | null r     = findNumbers n t rest
                         | otherwise  = e:r
    where r = findNumbers (n-1) (t-e) rest

solve :: Int -> Int -> String -> String
solve n t = show . product . findNumbers n t . map read . lines

main = do
    a <- getArgs
    let n = read (a !! 0)
    let t = read (a !! 1)
    interact (solve n t) <> putStrLn ""