import Data.Char
import Data.List
import System.IO
import GHC.IO.Encoding (getLocaleEncoding)
import System.Environment   

main = do  
    args <- getArgs
    putStrLn ("These two people match " ++ (show (dorrelation (args!!0) (args!!1))) ++ "%")


ordNames :: [Char] -> [Char] -> [Char]
ordNames x y
    | a < b = a ++ b
    | otherwise = b ++ a
    where a = map toLower x
          b = map toLower y

cntChar :: Eq a => [a] -> a -> Int
cntChar list elem = length (filter (\x -> x == elem) list)

cntAllCharInternal :: Eq a => [a] -> [a] -> [Int]
cntAllCharInternal [] oldList = []
cntAllCharInternal (x:xs) oldList = [(cntChar oldList x)] ++ (cntAllCharInternal xs oldList)

cntAllChar :: Eq a => [a] -> [Int]
cntAllChar str = cntAllCharInternal str str

zipListTogether :: [Int] -> [Int]
zipListTogether list
    | mod l 2 == 0  = removeDoubleDigit(zipWith (+) (take (div l 2) list) (take (div l 2) (reverse list)))
    | otherwise = removeDoubleDigit((zipWith (+) (take (div l 2) list) (take (div l 2) (reverse list))) ++ [list!!(div l 2)])
        where l = length list

removeDoubleDigit :: [Int] -> [Int]
removeDoubleDigit [] = []
removeDoubleDigit (x:xs)
    | l == 2 = [(div x 10), (mod x 10)] ++ (removeDoubleDigit xs)
    | otherwise = [x] ++ (removeDoubleDigit xs)
        where l = length (show x)

zipUntilPercent :: [Int] -> Int
zipUntilPercent list
    | l == 2 = (*) 10 (list!!0) + list!!1
    | l == 1 = list!!0
    | otherwise = zipUntilPercent (zipListTogether list)
        where l = length list

dorrelation :: String -> String -> Int
dorrelation a b = zipUntilPercent(cntAllChar (ordNames a b))