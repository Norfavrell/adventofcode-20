import System.Environment
import Data.List.Split

data TestCase n c = TestCase n n c [c] deriving (Show)
type Validator n c = TestCase n c -> Bool

parseTestCase :: Read l => String -> TestCase l Char
parseTestCase = (f . splitOneOf "-: ")
    where f (l1:l2:(c:[]):_:s:[]) = TestCase (read l1) (read l2) c s

isValid1 :: Eq c => Validator Int c
isValid1 (TestCase l1 l2 c p) = count <= l2 && count >= l1 --and $ map ($ count) [(>=l1), (<=l2)]
    where count = length . filter (==c) $ p

isValid2 :: (Eq c, Show c) => Validator Int c 
isValid2 t@(TestCase l1 l2 c p) = f $ map ((p !!) . (+ (-1))) [l1, l2]
    where f ar@(a:b:[]) = elem c ar && a /= b

solve :: Validator n c -> [TestCase n c] -> Int
solve v = sum . map (fromEnum . v)

main = do
    let solver t =  map (\x -> solve x t) [isValid1, isValid2]
    interact (show . solver . map parseTestCase . lines) <> putStrLn ""
