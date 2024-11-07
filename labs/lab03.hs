import Text.Printf (printf)


rotate::Int->[a]->[a]
rotate n xs = drop n xs ++ take n xs

sublist::Int->Int->[a]->[a]
sublist a b xs = drop a (take b xs)

addRemoveFirst :: [Int] -> [Int]
addRemoveFirst x = map (+head x) (drop 1 x)

biggerThan :: Ord a => a -> [a] -> [Bool]
biggerThan n = map (>n) 

listBiggerThan ::  Float -> [Float] -> [Float]
listBiggerThan n = filter (>n) 

takeN :: Int -> [a] -> [a]
takeN n [] = []
takeN n x = lastelms ++ replicate (max 0 (n - length (lastelms))) (last (lastelms))
    where lastelms = take n x

takeN' n x
    | n <=  length x  = (take n x)
    | otherwise =  x ++ replicate (n- (length x) ) (last x)

main :: IO ()
main = do
    print "Hi"
    
