
-- Exercise 1: Define a recursive function natp equivalent to the operator (*) for non-negative integers.
natpHelper :: Int -> Int -> Int -> Int
natpHelper n y summ = if n >0 then summ + natpHelper (n-1) y (summ+y) else 0

natp :: Int -> Int -> Int
natp x y = natpHelper x y 0

-- Exercise 2. Define a recursive function that returns the product of a list of Num.
muller :: [Int] -> Int
muller [] = 1
muller (x:xs) = x * muller xs

-- Exercise 3. Define a recursive function that returns lenght of a list.
lenner :: [Int] -> Int
lenner [] = 0
lenner (x:xs) = 1 + lenner xs

-- Exercise 4. Define a recursive function that returns the reverse of a list.

reverser :: [a] -> [a]
reverser [] = []
reverser (x:xs) = reverser xs ++ [x]

-- **Exercise 5**. Define a recursive function that inserts a new element of any ordered type into a sorted list to give another sorted list.

sortInsert :: (Eq a, Ord a) => a -> [a] -> [a]
sortInsert x [] = [x]
sortInsert x (y:ys) 
    | x <= y = x : y : ys
    | otherwise = y : sortInsert x ys

-- **Exercise 6**. Define a recursive function that sort a list (using previous function).

sorter :: (Eq a, Ord a) => [a] -> [a]
sorter [] = []
sorter (x:xs) = sortInsert x xs

-- Exercise 7. Define your own zip function using recursion.

zipper :: [a] -> [a] -> [(a,a)]
zipper _ [] = []
zipper [] _ = []
zipper (x:xs) (y:ys) = (x,y) : zipper xs ys


-- Exercise 8. Define your own drop function using recursion.
dropper :: Int -> [a] -> [a]
dropper _ [] = []
dropper n (x:xs) = if n >= 1 then dropper (n-1) xs else x:xs

--  Exercise 9. Define a recursive function that returns the nth Fibonacci number for any integer n>=0.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- Exercise 10. Define a recursive function that returns the Fibonnaci serie up to n

recFib :: Int -> [Int]
recFib 0 = [0]
recFib n = recFib (n-1) ++ [fib n]


-- Odd or Even: Mutual Recursion

evener :: Int -> Bool
evener 0 = True
evener n = odder (n-1)

odder :: Int -> Bool
odder 0 = False
odder n = evener (n-1)