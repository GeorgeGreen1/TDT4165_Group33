(!!) :: [a] -> Int -> a 
(!!) [] _ = error "Index too large" 
(!!) (x:xs) n 
    | n < 0 = error "Negative index" 
    | n == 0 = x 
    | otherwise = (Main.!!) xs (n-1)