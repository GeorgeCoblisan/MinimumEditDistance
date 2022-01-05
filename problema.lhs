\begin{code}

import System.IO  
import Data.List.Split


main :: IO ()
main = do  
     dataRead <- readFile "dictionar.txt"
     let dictionary = splitOn "," dataRead
     putStrLn "The dictionary is: "
     print dictionary
     let word = "sunday"
     putStrLn "Please enter the word: "
     word <- getLine
     let final = minFinal word dictionary
     print final


--Method 1
-- calculate levenshtein distance between two strings
levenshtein :: [Char] -> [Char] -> Int
levenshtein "" "" = 0
levenshtein s1 s2
  | length s2 == 0 = length s1
  | length s1 == 0 = length s2
  | head s1 == head s2 = levenshtein (tail s1) (tail s2)
  | otherwise = 1 + minimum [levenshtein (tail s1) s2,
                         levenshtein s1 (tail s2),
                         levenshtein (tail s1) (tail s2)]

minim :: [Int] -> Int
minim [] = 0
minim [x] = x
minim (x:xs) = min x (minim xs)

findWord:: [[Char]] -> [Int] -> Int -> [[Char]]
findWord [] [] m = []
findWord (x:xs) (a:an) m = if m == a then x : findWord xs an m else findWord xs an m

find:: [[Char]] -> [Char] -> [Int]
find [] s = []
find (x:xs) s = levenshtein x s : find xs s

minDistance:: [Char] -> [[Char]] -> [Int]
minDistance [] [] = []
minDistance s dictionary = find dictionary s
   

minFinal:: [Char] -> [[Char]] -> [[Char]]
minFinal s dictionary = findWord dictionary minList minNumber
    where 
        minList = minDistance s dictionary
        minNumber = minim minList



--Method 2
min3 :: Int -> Int -> Int -> Int
min3 x y z = min x (min y z)

cost :: Char -> Char -> Int
cost a b
    | a == b = 0
    | otherwise = 1

numberLetters :: String -> Char -> Int
numberLetters [] a = 0
numberLetters (s:ss) a = cost s a + numberLetters ss a

-- calculate levenshtein distance between two strings
levenshtein2 :: String -> String -> Int
levenshtein2 [] [] = 0
levenshtein2 s [] = numberLetters s '-'
levenshtein2 [] t = numberLetters t '-'
levenshtein2 (s:ss) (t:ts) = min3 (cost s t + levenshtein2 ss ts)
                                  (cost s '-' + levenshtein2 ss (t:ts))
                                  (cost '-' t + levenshtein2 (s:ss) ts)

find2:: [[Char]] -> [Char] -> [Int]
find2 [] s = []
find2 (x:xs) s = levenshtein2 x s : find2 xs s

minDistance2:: [Char] -> [[Char]] -> [Int]
minDistance2 [] [] = []
minDistance2 s dictionary = find2 dictionary s

minFinal2:: [Char] -> [[Char]] -> [[Char]]
minFinal2 s dictionary = findWord dictionary minList minNumber
    where 
        minList = minDistance2 s dictionary
        minNumber = minim minList



\end{code}
