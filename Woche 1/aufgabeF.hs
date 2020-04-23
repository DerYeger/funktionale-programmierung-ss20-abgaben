infixr ?
(?) minuend subtrahend
    | minuend < 0 = 0
    | subtrahend < 0 = 0
    | subtrahend > minuend = 0
    | otherwise = minuend - subtrahend
