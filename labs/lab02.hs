-- With guards
import Text.Printf

-- With guards
ticketPrice :: (Ord a1, Num a1, Fractional a2) => a1 -> a2
ticketPrice age
    | age > 60 = 0.45 * 9.0
    | age <= 5 = 0.4 * 9.0
    | age < 26 = 0.8 * 9.0
    | otherwise = 9.0


-- -- With patterns
ticketPrice' :: Int -> Double
ticketPrice' age | age > 60 = 0.45 * 9.0
ticketPrice' age | age <= 5 = 0.4 * 9.0
ticketPrice' age | age < 26 = 0.8 * 9.0
ticketPrice' _ = 9.0


secondsToHoursSeconds :: Int -> Int
secondsToHoursSeconds seconds = mod (mod seconds 3600) 60

secondsToHoursMinutes :: Int -> Int
secondsToHoursMinutes seconds = div (mod seconds 3600 - secondsToHoursSeconds seconds) 60

secondsToHoursHours :: Int -> Int
secondsToHoursHours seconds  = div seconds 3600

secondsToHours :: Int -> String
secondsToHours seconds = printf "%02d:%02d:%02d" (secondsToHoursHours seconds) (secondsToHoursMinutes seconds) (secondsToHoursSeconds seconds)



quadrant :: Int -> Int -> String
quadrant x y 
    | x == 0 || y == 0 = "The values are not valid"
    | x > 0 && y > 0 = "1st"
    | x < 0 && y > 0 = "2nd"
    | x < 0 && y < 0 = "3rd"
    | otherwise = "4th"
            



leapYearHelper :: Int -> Bool
leapYearHelper year = if mod year 100 == 0 then mod year 400 == 0 else mod year 4 == 0

leapYearTense :: Int -> String
leapYearTense year
    | year == 2022 = " is "
    | year > 2022 = " will be "
    | otherwise = " was "

leapYear :: Int -> String
leapYear year = show year ++ leapYearTense year ++ if leapYearHelper year then "a leap year" else "not a leap year"

calculator::Int->Int->String->Int
calculator num1 num2 operator
    | operator == "+" = num1 + num2
    | operator == "*" = num1 * num2
    | operator == "-" = num1 - num2
    | operator == "div" = div num1 num2
    | otherwise = error "wrong operator"

main :: IO ()
main = do
    let result = ticketPrice 25
    putStrLn $ "The ticket price is: " ++ show result