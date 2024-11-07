data Student = Student String String [Float] deriving Show

getFirstName:: Student -> String
getFirstName (Student firstName lastName markList) = firstName

getLastName:: Student -> String
getLastName (Student firstName lastName markList) = lastName

maxi :: Student -> Float
maxi (Student firstName lastName markList) = maximum markList

mini :: Student -> Float
mini (Student firstName lastName markList) = minimum markList


mean :: Student -> Float
mean (Student firstName lastName markList) =  sum markList / fromIntegral (length markList)

count :: Student -> Int
count (Student firstName lastName markList) = length (filter (>=5) markList)

data Date = Date  Int Int Int deriving Show

instance Eq Date where
    (Date d1 m1 y1) == (Date d2 m2 y2) = d1 == d2 && m1 == m2 && y1 == y2

instance Ord Date where
    (Date d1 m1 y1) <= (Date d2 m2 y2) = y1 < y2 || (y2 == y1 && m1 < m2) || (y2 == y1 && m1 == m2 && d1 <= d2)


isleap :: Date -> Bool
isleap (Date day month year) = 
    year `mod` 400 == 0 || (year `mod` 4 == 0 && year `mod` 100 /= 0)

rightMonth :: Date -> Bool
rightMonth (Date day month year) = (month >= 1) && (month <= 12)

rightDay :: Date -> Bool
rightDay (Date day month year) 
    | month == 2 = (day >= 1) && ((isleap (Date day month year) && day <= 29) || (day <= 28))
    | month == 1 = (day >= 1) && (day <= 31)
    | month == 3 = (day >= 1) && (day <= 31)
    | month == 5 = (day >= 1) && (day <= 31)
    | month == 7 = (day >= 1) && (day <= 31)
    | month == 8 = (day >= 1) && (day <= 31)
    | month == 10 = (day >= 1) && (day <= 31)
    | month == 12 = (day >= 1) && (day <= 31)
    | otherwise = (day >= 1) && (day <= 30)

checkDate :: Date -> Bool
checkDate (Date day month year) = (rightDay (Date day month year)) && (rightMonth (Date day month year))

nextDay :: Date -> Date
nextDay (Date day month year)
    | checkDate (Date (day+1) month year) = (Date (day + 1) month year)
    | checkDate (Date 1 (month+1) year) = (Date 1 (month+1) year)
    | otherwise = (Date 1 1 (year + 1)) 

prevDay :: Date -> Date
prevDay (Date day month year)
    | checkDate (Date (day-1) month year) = (Date (day - 1) month year)
    | checkDate (Date 31 (month-1) year) = (Date 31 (month-1) year)
    | checkDate (Date 30 (month-1) year) = (Date 30 (month-1) year)
    | checkDate (Date 29 (month-1) year) = (Date 29 (month-1) year)
    | checkDate (Date 28 (month-1) year) = (Date 28 (month-1) year)
    | otherwise = (Date 31 12 (year - 1))
    
instance Enum Date where
    succ (Date day month year) = nextDay (Date day month year)
    pred (Date day month year) = prevDay (Date day month year)


data Product = Product String Float Int | FreshProduct String Float Int Date deriving Show

sell :: Product -> Product
sell (Product name price quantity) = Product name price (if quantity > 0 then quantity -1 else 0)
sell (FreshProduct name price quantity expiry) = FreshProduct name price (if quantity > 0 then quantity -1 else 0) expiry

expired :: Date -> Product -> Bool
expired date1 (FreshProduct name price quantity expdate) = date1 >= expdate
expired date2 (Product name price quantity) = False

instance Eq Product where
    (Product name1 price1 quantity1) == (Product name2 price2 quantity2) = name1 == name2 && price1 == price2
    (FreshProduct name1 price1 quantity1 date1) == (FreshProduct name2 price2 quantity2 date2) = name1 == name2 && price1 == price2 && date1 == date2

