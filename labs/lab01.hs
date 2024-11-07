addCents :: Int -> Int -> Int -> Int -> Int -> Int -> Double
addCents c1 c2 c5 c10 c20 c50 = (0.5 * fromIntegral c50) + (0.2 * fromIntegral c20) + (0.1 * fromIntegral c10) + (0.05 * fromIntegral c5) + (0.02 * fromIntegral c2) + (0.01 * fromIntegral c1)

maxOf3:: Int -> Int -> Int -> Int
maxOf3 a b c = if a >= b && a >= c then a else if b >= a && b >= c then b else c

middle :: Int -> Int -> Int -> Int
middle a b c
    | (a <= b && b <= c) || (c <= b && b <= a) = b
    | (b <= a && a <= c) || (c <= a && a <= b) = a
    | otherwise = c

allEqual :: Eq a => a -> a -> a -> a -> Bool
allEqual a b c d = a == b && b == c && c == d

allDifferent :: Eq a => a -> a -> a -> a -> Bool
allDifferent a b c d = a /= b && b /= c && c /= d

allDifferent' :: Eq a => a -> a -> a -> a -> Bool
allDifferent' a b c d = a /= b && b /= c && c /= d

combine :: Num a => a -> a -> a
combine a b = (a * 10) + b


highestNumber a b = if a > b then combine a b else combine b a

main :: IO ()
main = do
    let result = addCents 1 2 3 4 5 6
    putStrLn $ "The total amount is: " ++ show result