{-
CPSC 449 Assignment 2
By: Evan Wheeler
ID# 30046173
-}

import Data.List

--Question 1
productFactorial :: Int -> Int
productFactorial 0 = 1
productFactorial x = factorial x * productFactorial (x - 1)


factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)


--Question 2
smallestFactor :: Integer -> Integer
smallestFactor 1 = 1
smallestFactor x = head [y | y <- [2..x], y `isFactorOf` x]

--Helper to determine if the first Integer is a factor of the Second
isFactorOf :: Integer -> Integer -> Bool
isFactorOf m n = n `mod` m == 0


--Question 3
gameOddEven :: Integer -> [Integer]
gameOddEven x
            | x == 1    = [1]
            | otherwise = x: gameOddEven(gameNextNum x)

--Heleper that returns the next number in the game given the current number
gameNextNum :: Integer -> Integer
gameNextNum x
         | x `mod` 2 == 0 = x `div` 2
         | otherwise      = 3 * x + 1


--Question 4
isGoodPassword :: String -> Bool
isGoodPassword n = ((length n >= 8) && (passwordHasLowercase n) && (passwordHasUppercase n) && (passwordHasNumbers n))

--------Helpers--------
passwordHasLowercase :: String -> Bool
passwordHasLowercase x = any (`elem` ['a','b'..'z']) x

passwordHasUppercase :: String -> Bool
passwordHasUppercase x = any (`elem` ['A','B'..'Z']) x

passwordHasNumbers :: String -> Bool
passwordHasNumbers x = any (`elem` ['0','1'..'9']) x


--Question 5
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime x = null [n | n <- [2..x-1], isFactorOf n x] --If the list of factors is empty the number is prime


--Question 6
allDivisors :: Integer -> [Integer]
allDivisors x = [y | y <- [1..x], isFactorOf y x]


--Question 7
{-
  I found the wording of this question ambiguous, when it says "picks out all occurences
of an integer n in a list".
  I took that to mean that it should create a new list with only
values that match the given integer. However, this function seems fairly useless because "numOccurences"
would give you what you want by saying how many times the value occurs in the list.
  If the question is supposed to be interpreted that "picks out" means filters the list and removes all of the n
values from it I have supplied code for that just below my code that can be commented in/ out if that is what the
test cases are written for.
-}
--Filters out non 'n' values to leave a list of only n's
matches :: Integer -> [Integer] -> [Integer]
matches n (x) = filter (\y -> y == n) x

--Un-comment the lines below if the question is supposed to filter out n instead of values that aren't n
--matches :: Integer -> [Integer] -> [Integer]
--matches n (x) = filter (\y -> y /= n) x


--Question 8
solveQuadraticEquation :: Double -> Double -> Double -> (Double , Double)
solveQuadraticEquation a b c = ( ((-b) + discriminant ) / (2 * a), ((-b) - discriminant ) / (2 * a) )
                       where discriminant = sqrt(b ** 2 - 4 * a * c)


--Question 9
occursIn :: Eq a => a -> [a] -> Bool
occursIn _ [] = False
occursIn n (x:xs)
         | n == x    = True
         | otherwise = occursIn n xs


--Question 10
allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn [] (y) = True
allOccurIn (x:xs) (y)
           | occursIn x y = allOccurIn xs y
           | otherwise    = False


--Question 11
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements x y = (allOccurIn x y)&&(allOccurIn y x)


--Question 12
numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences _ []        = 0 --Second list is empty == base case
numOccurrences n (x: xs)
               | x == n    = 1 + (numOccurrences n xs)
               | otherwise = numOccurrences n xs


--Question13
allUrls :: String -> [String]
allUrls x = filter isUrl (words x)

--Helper that takes a string and checks if it is a url
isUrl :: String -> Bool
isUrl x = isPrefixOf "http://" x


--Question 15
{-Note, the question specified that we use the 'Int' data type for this function.
This is not a problem up to "pascal 20" but after that point, the calculations for factorials of Int's overflows
and wraps around to return negative numbers. Using an 'Integer' would stop this but so long as tha test cases are
less than pascal 21 there will be no problem.
  Also, the fucntion assumes that 'pascal 0' == [1] is the 'first row'. This means "pascal 1" will return [1,1]
  because it is the second row.
-}
pascal :: Int -> [Int]
pascal n = map (calcPascalValue n) [1..n+1]

--Helper that calculates the value of the x'th value in row n in pascal's triangle
calcPascalValue :: Int -> Int -> Int
calcPascalValue n x = (factorial (n)) `div` (factorial(n - (x-1)) * factorial (x - 1))




fastPascal :: Integer -> [Integer]
fastPascal n = map (fastCalcPascalValue n) [1..n+1]

--Helper that calculates the value of the x'th value in row n in pascal's triangle
fastCalcPascalValue :: Integer -> Integer -> Integer
fastCalcPascalValue n x = (fastFact (n)) `div` (fastFact(n - (x-1)) * fastFact (x - 1))


fastFact :: Integer -> Integer
fastFact n = product [2..n]