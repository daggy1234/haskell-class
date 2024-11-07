-- List with the cubes of the first 20 numbers
list1 = [ x^3 | x <- [1..20]]
-- Infinite list with cubes. Don't print it! 
list2 = [ x^3 | x <- [1..]]

-- Exercise 1. Use list comprehension to create the function replicate' n x that creates a list with n copies of x.

replicate' n itm  = [itm | _ <- [1..n]]


-- Exercise 2. Create a function takeSquaresCubes n x that creates a list of tuples with the n first multiples of a number x, their squares and cubes

takeSquaresCubes n y  = take n [(x, x^2, x^3) | x <- [y..], x `rem` y == 0]

-- Exercise 3. Create a function additionSquaresCubes n x where the squares and cubes of the n first multiples of x are added. Consider reusing the previous function.

additionSquaresCubes n y  = take n [x^2 + x^3 | x <- [y..], x `rem` y == 0]

-- Exercise 4. Create a function additionSquareCubes' n x e where the squares and cubes of the n first multiples of x are added, only if any of them finishes with e.

additionSquareCubes' :: Int -> Int -> Int -> [Int]
additionSquareCubes' n y e = take n [x^2 + x^3 | x <- [y..(y * n)], x `rem` y == 0, ((x^2 `rem` 10) == e) || ((x^3 `rem` 10) == e)]

-- Exercise 5. Create a list of tuples with the multiplication tables of 1 to 10 as in [(1,1,1), (1,2,2), (1,3,3)...]

tables = concat [[(y,x,y*x) | x <- [1..10]] | y <- [1..10]]

-- Exercise 6. Create a consecutive xs function that given a list of elements tells if they are consecutive either in ascending or descending order. Which is its type? Tip: use the pairs function seen in lectures.

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

consecutive :: (Enum a, Eq a) => [a] -> Bool
consecutive l =  and [succ x == y | (x,y) <- pairs l]


