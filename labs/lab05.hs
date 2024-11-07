type Date = (Int, Int, Int)

isleap :: Date -> Bool
isleap (day,month,year) = 
    year `mod` 400 == 0 || (year `mod` 4 == 0 && year `mod` 100 /= 0)

rightMonth :: Date -> Bool
rightMonth (day, month, year) = (month >= 1) && (month <= 12)

rightDay :: Date -> Bool
rightDay (day, month, year) 
    | month == 2 = (day >= 1) && ((isleap (day, month, year) && day <= 29) || (day <= 28))
    | month == 1 = (day >= 1) && (day <= 31)
    | month == 3 = (day >= 1) && (day <= 31)
    | month == 5 = (day >= 1) && (day <= 31)
    | month == 7 = (day >= 1) && (day <= 31)
    | month == 8 = (day >= 1) && (day <= 31)
    | month == 10 = (day >= 1) && (day <= 31)
    | month == 12 = (day >= 1) && (day <= 31)
    | otherwise = (day >= 1) && (day <= 30)


fixDate :: Date -> Date
fixDate (day, month, year)
    | not (rightMonth (day, month, year)) && not (rightDay (day, month, year)) = (1,1,year)
    | not (rightDay (day, month, year)) = (1, month, year)
    | not (rightMonth (day, month, year)) = (day, 1, year)
    | otherwise = (day, month, year)

monthName :: Date -> String
monthName (day, month, year) 
   | month < 1 || month > 12 = error "Wrong month"
   | month == 1 = "january"
   | month == 2 = "february"
   | month == 3 = "march"
   | month == 4 = "april"
   | month == 5 = "may"
   | month == 6 = "june"
   | month == 7 = "july"
   | month == 8 = "august"
   | month == 9 = "september"
   | month == 10 = "october"
   | month == 11 = "november"
   | month == 12 = "december"


showDate :: Date -> String
showDate (day, month, year) = show day ++ " of " ++ monthName (day, month, year) ++ " of the" ++ (if not (isleap (day, month, year)) then " not " else " ") ++ "leap year " ++ show year

checkDate :: Date -> Bool
checkDate (day, month, year) = (rightDay (day, month, year)) && (rightMonth (day, month, year))

nextDay :: Date -> Date
nextDay (day, month, year)
    | checkDate (day+1, month, year) = (day + 1, month, year)
    | checkDate (1, month+1, year) = (1, month+1, year)
    | otherwise = (1, 1, year + 1) 


prevDay :: Date -> Date
prevDay (day, month, year)
    | checkDate (day-1, month, year) = (day - 1, month, year)
    | checkDate (31, month-1, year) = (31, month-1, year)
    | checkDate (30, month-1, year) = (30, month-1, year)
    | checkDate (29, month-1, year) = (29, month-1, year)
    | checkDate (28, month-1, year) = (28, month-1, year)
    | otherwise = (31, 12, year - 1) 
    
isPrevious :: Date -> Date -> Bool
isPrevious (daya,montha,yeara) (dayb, monthb, yearb)
    | yeara > yearb = False
    | montha > monthb = False
    | daya >= dayb = False
    | otherwise = True

