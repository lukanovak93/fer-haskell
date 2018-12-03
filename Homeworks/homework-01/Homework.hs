module Homework where
--
import Data.List
import Data.Char
--

-- Task 01

isLeapYear :: Int -> Bool
isLeapYear x
  | x `mod` 400   == 0 = True
  | x `mod` 4     == 0  && x `mod` 100 /= 0 = True
  | otherwise     = False

leapList :: [Int]
leapList = [x | x <- [1996..2017], isLeapYear x]

-- Task 02
evaluate :: Double -> [Double] -> Double
evaluate x coef = sum $ [c * x^e | (c, e) <- zip coef [0..]]

factorial :: Double -> Double
factorial x = product $ [1..x]

maclaurin :: [Double]
maclaurin = [1 / factorial x | x <- [0..]]

exp' :: Double -> Double
exp' x = evaluate x $ take 170 $ maclaurin

-- Task 03
findItem :: [(String, a)] -> String -> [(String, a)]
findItem xs x = [a | a <- xs, fst a == x]

contains :: [(String, a)] -> String -> Bool
contains xs x = not $ null $ [a | a <- xs, fst a == x]

lookup :: [(String, a)] -> String -> a
lookup xs x
  | contains xs x   = snd $ head $ findItem xs x
  | otherwise       = error "Key doesn't exist"

insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert xs x
  | not $ contains xs $ fst x   = x:xs
  | otherwise                   = xs

remove :: [(String, a)] -> String -> [(String, a)]
remove xs x
  | contains xs x   = [a | a <- xs, fst a /= x]
  | otherwise       = xs

update :: [(String, a)] -> String -> a -> [(String, a)]
update xs key value
  | contains xs key   = (key, value) : remove xs key
  | otherwise         = xs

-- Task 04
lowerWord :: String -> String
lowerWord xs = concat $ [[toLower x] | x <- xs, isLetter x]

toLowerWords :: String -> [String]
toLowerWords xs = [lowerWord x | x <- words xs]

countOccurances :: (Num a, Eq a) => String -> String -> a
countOccurances input word = sum $ [1 | a <- toLowerWords input, a == lowerWord word]

getInputVector :: (Num a, Eq a) => String -> [(String, a)]
getInputVector input = nub $ [(word, countOccurances input word) | word <- toLowerWords input]

getVectorSpace :: (Num a, Eq a) => [(String, a)] -> [(String, a)] -> [(String, a, a)]
getVectorSpace s1 s2 = nub $ [(w, Homework.lookup s1 w, Homework.lookup s2 w) | (w, value) <- nub $ (s1 ++ s2), contains s1 w, contains s2 w]

getFst :: (a, b, c) -> b
getFst (_,b,_) = b

getSnd :: (a, b, c) -> c
getSnd (_,_,c) = c

getNumerator :: (Num a, Eq a) => String -> String -> a
getNumerator s1 s2 = sum $ [getFst w * getSnd w | w <- getVectorSpace (getInputVector s1) (getInputVector s2)]

getNorm :: String -> Double
getNorm xs = sqrt $ sum $ [occ^2 | (word, occ) <- getInputVector xs]

getDenominator :: String -> String -> Double
getDenominator s1 s2 = getNorm s1 * getNorm s2

cosineSimilarity :: String -> String -> Double
cosineSimilarity s1 s2 = getNumerator s1 s2 / getDenominator s1 s2
