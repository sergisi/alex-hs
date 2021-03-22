isLucky :: Int -> Bool
isLucky n = and [
                even $ length ticket, 
                (sum $ fst $ splitAt (length ticket `div` 2) ticket) == (sum $ snd $ splitAt (length ticket `div` 2) ticket)
                ] 
    where ticket = fromDigitToArray n


fromDigitToArray :: Int -> [Int]
fromDigitToArray 0 = []
fromDigitToArray x = fromDigitToArray (x `div` 10) ++ [x `mod` 10]