
--J.T. Lomenick
--CSCI 555: Functional Programming
--Dr. Conrad Cunningham
--6 September 2010

--Homework 1

--This function takes too Booleans and returns the
--"exclusive or" of the two values

xor :: Bool -> Bool -> Bool
xor x y = (x && (not y)) || ((not x) && y)  --an equivalency for xor

--This function takes two natural numbers and returns their product
--without using  '*' or '/'

mult :: Int -> Int -> Int
mult x y 
	| x == 0 = 0
	| x /= 0 = y + mult (x-1) y

--This function returns the maximum value of a non-empty list

maxval :: Ord a => [a] -> a
--maxval :: [Int] -> Int
maxval (x:y:xs)
	| x >= y = maxval (x:xs)
	| x < y  = maxval (y:xs)
maxval (x:xs) = x  

--This function takes a list and returns the list of
--all pairs of adjacent elements

adjpairs :: Eq a => [a] -> [(a,a)]
--adjpairs :: [Int] -> [(Int, Int)]
adjpairs (x:y:xs) = (x,y) : adjpairs(y:xs)
adjpairs (x:xs) = []


--This function takes a list of Integers and returns
--the mean of the values
sumlist :: [Int] -> Int
sumlist (x:xs) = x + sum xs

count :: [Int] -> Int
count (x:xs) = 1 + count xs
count xs = 0


mean :: [Int] -> Int
mean [] = 0
mean xs = (sumlist xs) `div` (count xs) 

