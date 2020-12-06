
import Text.ParserCombinators.Parsec
import Text.Parsec.Perm
import Data.List.Split
import Data.Either
import Text.Printf
import System.Environment

-- |TODO This is a complete mess, come back and refactor this at some point. Use Applicative form....

type Year = Int
type Color = String
type HairColor = String
type EyeColor = String
type Height = (Int, String)

data Passport = Passport {
    birthYear       :: Year,
    issueYear       :: Year,
    expirationYear  :: Year,
    height          :: Height,
    hairColor       :: HairColor,
    eyeColor        :: Color,
    passportID      :: String,
    countryID       :: Maybe Int
} deriving (Show)

passport :: Parser Passport
passport = permute (
    Passport <$$> f "byr" year
             <||> f "iyr" year
             <||> f "eyr" year
             <||> f "hgt" len
             <||> f "hcl" color
             <||> f "ecl" color
             <||> f "pid" (many1 (noneOf "\n "))
             <|?> f' "cid" ident)
    where f s p  = try (string $ s++":") >> p <* sep
          f' s p = (Nothing, Just <$> f s p)
          year   = read <$> many1 digit
          ident  = read <$> many1 digit
          color  = many1 (noneOf "\n ")
          len    = (,) <$> (read <$> many1 digit) <*> (string "cm" <|> string "in" <|> string "")
          sep    = skipMany1 (char '\n' <|> char ' ') <|> eof


year :: Int -> Int -> Parser Year
year min max = do
    val <- count 4 digit
    let ival = read val :: Int
    if ival < min || ival > max
        then fail (printf "Year '%d' should be between %d and %d" ival min max)
        else return ival

heightPars :: Parser Height
heightPars = do
    h@(v,u) <- (,) <$> (read <$> many1 digit) <*> (string "cm" <|> string "in")
    case u of
        "cm" -> if v < 150 || v > 193 then fail (printf "Height '%s' should be between %dcm and %dcm" (show h) (150::Int) (193::Int)) else return h
        "in" -> if v < 59 || v > 76 then fail (printf "Height '%s' should be between %din and %din" (show h) (59::Int) (76::Int)) else return h
        _    -> fail (printf "Unknown unit %s in height %s" u (show h))

hairColorParser :: Parser HairColor
hairColorParser = (++) <$> string "#" <*> count 6 hexDigit

eyeColorParser :: Parser EyeColor
eyeColorParser = do
    val <- many1 letter
    if val `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
        then return val
        else fail (printf "Invalid eye color '%s'" (show val))

pidParser :: Parser Int
pidParser = read <$> count 9 digit

passportStrict :: Parser Passport
passportStrict = permute (
    Passport <$$> f "byr" (year 1920 2002)
             <||> f "iyr" (year 2010 2020)
             <||> f "eyr" (year 2020 2030)
             <||> f "hgt" heightPars
             <||> f "hcl" hairColorParser
             <||> f "ecl" eyeColorParser
             <||> f "pid" (show <$> pidParser)
             <|?> f' "cid" ident)
    where f s p  = try (string $ s++":") >> p <* sep
          f' s p = (Nothing, Just <$> f s p)
          ident  = read <$> many1 digit
          sep    = skipMany1 (char '\n' <|> char ' ') <|> eof


parsePassports :: Parser Passport -> String -> [Either ParseError Passport]
parsePassports p = map (\x -> parse p x x) . splitOn "\n\n"

main = do
    args <- getArgs
    inData <- readFile $ head args

    let solution1 = length $ filter isRight $ parsePassports passport inData
    let solution2 = length $ filter isRight $ parsePassports passportStrict inData

    print solution1
    print solution2