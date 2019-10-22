romanBase = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"] 
arabicBase = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
romanArabicKey = zip romanBase arabicBase

romanCalc :: [Char] -> Float 
-- Inputs:
--   str (string) : Two roman numerals and an operation of the form "II + XI" or "MDI / III"
-- Output:
--   The result of the operation on the roman numerals, in arabic form
romanCalc str | op == "+" = num1 + num2
              | op == "*" = num1 * num2
              | op == "-" = num1 - num2
              | op == "/" = num1 / num2 -- This is repetitive but I don't know how to paste in the operation otherwise
                where (num1, op, num2) = (1,"+",1)

-- strSplit (x:'+':y) = (x,'+',y)
-- strSplit str = error "The input does not contain any operation (+, *, -, /)"

strSplit :: [Char] -> ([Char], Char, [Char])
-- Inputs:
--   str (string)
-- Outputs:
--   (part1, op, part2), where part1 is the first part of the string (before any of +, *, - or / is found in str) and part2 is the last part of the string (after the operation)
strSplit str = case break (== '+') str of -- How to take care of all operations in one go? Use elem?
          (s, _:r) -> (s,'+',r)
          _        -> error "No operation found in input (must be one and only one of +, *, - or /)"

isRoman :: [Char] -> Bool
-- Inputs:
--   str (string)
-- Output:
--   True if str is a valid roman numeral
--   False otherwise
-- isRoman str = not (or (map (\x -> contains x str) forbiddenRoman)) 
isRoman str = and [ x `elem` romanBase | x <- substr str 1 ++ substr str 2 ]
forbiddenRoman = ["IIII", "VV", "XXXX", "LL", "CCCC", "DD"] ++ [ [x] | x <- ['A'..'Z'] ++ ['1'..'9'], x `notElem` ['M', 'D', 'C', 'L', 'X', 'V', 'I'] ]

-- divide str into 1-substrings and 2-substrings. Put all of those in a list. For each one, ask: is this in the romanBase?
-- [ x `elem` romanBase | x <- substr str 1 ++ substr str 2 ]
-- If yes, continue:
-- ask whether there is any string in ["IIII", "VV", "XXXX", "LL", "CCCC", "DD"]which is contained in str
-- What about IVI?

romanToArabic :: [Char] -> Int
-- Inputs:
--   str (string): A roman numeral
-- Output:
--   The corresponding number in arabic form (1, 2, ...)
romanToArabic str | not (isRoman str) = error (str ++ " is not a valid roman numeral!")
                  | otherwise = 1

arabicToRoman :: Int -> [Char]
-- Inputs:
--   n (integer)
-- Output:
--   The corresponding number in roman numeral form ("I", "II", ...)
arabicToRoman n = dotChar (baseDecomposition n arabicBase) romanBase
-- use sequential eclidean division with fold to form an improper roman numeral (with "IIII" and such) and filter the result 

dotChar :: [Int] -> [[Char]] -> [Char]
-- Inputs:
--   A list of integers, base
--   A list of strings, strList
-- Outputs:
--   A string corresponding to the scalar product of base and strList
-- E.g. dotChar [2,0,1] ["MA","x","N"] => "MAMAN"
dotChar [] _ = []
dotChar (x:xs) (y:ys) | length xs /= length ys = error "The inputs are not the same size"
                      | otherwise = strMult x y ++ dotChar xs ys

strMult :: Int -> [Char] -> [Char]
-- Inputs:
--   n (integer)
--   str (string)
-- Outputs:
--   The concatenation of str, n times
-- E.g. strMult 2 "MA" => "MAMA"
strMult 0 _   = ""
strMult n str = str ++ (strMult (n-1) str)

baseDecomposition :: Int -> [Int] -> [Int]
-- Inputs:
--   n (integer)
--   base (list of integers)
-- Outputs:
--   The digits of n in the given base
-- E.g.: baseDecomposition 115 [15,2,1] => [7,5,0]
baseDecomposition n []   = []
baseDecomposition n base | 1 `notElem` base = error "The decomposition will be incomplete" -- Maybe just throw a warning?
                         | otherwise = (n `div` (head base)) : ( baseDecomposition (n `mod` (head base)) (tail base) )

contains :: [Char] -> [Char] -> Bool
-- Inputs:
--   str1 (string)
--   str2 (string)
-- Outputs:
--   True if str2 contains str1, False otherwise
contains [] str2   = True
contains str1 str2 = elem str1 (substr str2 (length str1))

substr :: [Char] -> Int -> [[Char]]
-- Inputs:
--   str (string)
--   n (integer)
-- Outputs:
--   A list of the consecutive substrings of str that are n elements long
-- E.g.: substr "ABCDE" 2 => ["AB", "BC", "CD", "DE"]
substr str 0 = []
substr str n = if length nSub == n
               then nSub : ( substr (drop 1 str) n )
               else []
               where nSub = take n str
