year_days year | year `rem` 400 == 0 = 366
               | year `rem` 100 == 0 = 365
               | year `rem` 4 == 0 = 366
               | otherwise = 365

-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
month_days _ 1 = 31
month_days y 2 | year_days y == 365 = 28
               | year_days y == 366 = 29
month_days _ 3 = 31
month_days _ 4 = 30
month_days _ 5 = 31
month_days _ 6 = 30
month_days _ 7 = 31
month_days _ 8 = 31
month_days _ 9 = 30
month_days _ 10 = 31
month_days _ 11 = 30
month_days _ 12 = 31

days_since1900 year month day
    | day > 1 = day' +
                (days_since1900 year month 1)
    | month > 1 = (month_days year month') +
                  (days_since1900 year month' day)
    | year > 1900 = (year_days year') +
                    (days_since1900 year' month day)
    | day == 1 && month == 1 && year == 1900 = 0
    where year' = year - 1
          month' = month - 1
          day' = day - 1

wrap_wday wday | wday > 6 = wday `rem` 7
               | wday < 0 = -(wday `rem` 7)
               | otherwise = wday

euler19 = length first_sundays
    where first_sundays = filter ((==) 6) first_wdays
          first_wdays = map wrap_wday first_days
          first_days = [days_since1900 y m 1 | y <- [1901..2000], m <- [1..12]]
