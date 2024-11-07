import Data.Char

bin2int :: [Int] -> Int
bin2int bits = sum [b * 2^i | (i,b) <- zip [0..] (reverse bits)]


bin2intfl :: [Int] -> Int
bin2intfl bits = foldl (\rsum x -> x + (2 * rsum)) 0 bits

helper :: Int -> [Int]
helper 0 = []
helper n = (n `mod` 2) : helper (n `div` 2)
    
int2bin :: Int -> [Int]
int2bin 0 = [0]
int2bin n = reverse (helper n)

make8 :: [Int] -> [Int]
make8 bits 
    | length bits == 8 = bits
    | length bits > 8 = take 8 bits
    | length bits < 8 = (take (8 - (length bits)) (repeat 0)) ++ bits


encode :: String -> [Int]
encode =  concatMap (make8 . int2bin . ord)


chop8 :: [Int] -> [[Int]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)


decode :: [Int] -> String
decode = map (chr . bin2int) . chop8

channel :: a -> a
channel = id

transmit :: String -> String
transmit = decode . channel . encode

main :: IO ()
main = putStrLn (transmit "Haskell is fun")