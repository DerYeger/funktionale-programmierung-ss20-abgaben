myAdd :: (Int, Int) -> Int
myAdd (x, y) = x + y

add :: Int -> Int -> Int
add x y = x + y

-- myInc = myAdd 1
myWorkingInc = add 1

main = interact result
    where result input = show $ myWorkingInc 41
