

sort xs = f [] xs where
    f ms [] = merge ms
    f ms [x] = merge ([x]:ms)
    f ms (x:y:rs) = if x <= y then g ms x [] y [] rs else g ms y [] x [] rs
    g ms x xs y ys (r:rs)
        | r <= x = g ms r (x:xs) y ys rs
        | r > y = g ms x xs r (y:ys)
    g ms x xs y ys rs = f ((x:xs):(reverse (y:ys)):ms) rs
