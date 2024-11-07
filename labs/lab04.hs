import Prelude
-- **Exercise 1.** Use list comprehension to create the function `evenPositions xs` that returns the position of all the even elements in a list.

evenPositions :: [Int] -> [Int]
evenPositions xs = [i | i <- [0..(length xs - 1)], even (xs!!i)]

-- Exercise 2. Create a posFirstEven function that returns the position of the first even element of a list or -1 if there are no even elements in the list.

posFirstEven :: [Int] -> Int
posFirstEven xs = if null evenPos then -1 else head evenPos where evenPos = evenPositions xs

-- Exercise 3. Use the previous function to create a splitEven function that splits a list at its first even element (it will be part of the second list)

splitEven :: [Int] -> ([Int], [Int])
splitEven xs = (take listPivot xs, drop listPivot xs) 
    where pivot = posFirstEven xs
          listPivot = if pivot == -1 then length xs else pivot

-- Exercise 4. Create a function count x xs that counts the number of times an element x appears in list xs.

count :: (Eq x) => x -> [x] -> Int
count x xs = sum [1 | elm <- xs, elm == x]


-- Ercise 5: A number is perfect if it is equal to the addition of all its divisors, except itself. Create the perfectTo x function that returns a list with all the perfect numbers until the given one (included). Note: auxiliary functions can be created.

perfectCheck :: Int -> Bool
perfectCheck n = n == sum [divisor | divisor <- [1..(n-1)], (n `rem` divisor) == 0]

perfectTo :: Int -> [Int]
perfectTo limit = [lim | lim <- [1..limit], perfectCheck lim]

-- Exercise 6. Create the multiples3_5 n function that returns the addition of all the multiples of 3 or 5 to the number n included.

multiples3_5 :: Int -> Int
multiples3_5 n = sum [x | x <- [1..n], (x `rem` 5) == 0 || (x `rem` 3) == 0]