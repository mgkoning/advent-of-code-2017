
take5 x = take 5 x [] where
    take _ [] ys = ys
    take 0 _ ys = ys
    take n (x:xs) ys = take (n-1) xs (x:ys)