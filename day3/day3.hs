import qualified Data.Vector as V

data Cell = Empty | Tree deriving (Show, Eq)
type Move = (Int, Int)
type Position = (Int, Int)
type GridSize = (Int, Int)
type Grid a = V.Vector (V.Vector a)

-- |Parse the input map into a 2d grid
parseMap :: String -> Grid Cell
parseMap i = V.fromList $ map (V.fromList . map f) $ lines i
    where f '.' = Empty
          f '#' = Tree

-- |Solve the first part of the problem 
solveSingle :: Grid Cell -> Move -> Int
solveSingle g m = countTrees $ map getCell $ walk (my, mx) (0, 0) m
    where my      = V.length g
          mx      = V.length $ V.head g
          getCell    = \(x,y) -> (g V.! y) V.! x
          countTrees =  length . filter (==Tree)

solveMany :: Grid Cell -> [Move] -> Int
solveMany g m = product $ map (solveSingle g) m 

walk :: GridSize -> Position -> Move -> [Position]
walk g@(hei, wid) p@(x, y) m@(dx, dy) | ny >= hei  = []
                                      | otherwise  = np:walk g np m
    where nx = mod (x+dx) wid
          ny = y+dy
          np = (nx, ny)

main = do
    inputData <- readFile "day3/data/input.in"
    let inputMap = parseMap inputData

    let validMoves = [(1,1), (3,1), (5,1), (7,1), (1,2)] :: [(Int, Int)]
    let sol = solveMany inputMap validMoves
    print(sol)