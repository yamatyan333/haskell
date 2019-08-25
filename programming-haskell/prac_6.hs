and2 :: [Bool] -> Bool
and2 (x:xs) | x = and xs
           | otherwise = False

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ concat xs

replicate2 :: Int-> b -> [b]
replicate2 0 _ = []
replicate2 n x = x:(replicate (n-1) x)

index2 :: [a] -> Int -> a
index2 (x:xs) n | n == 0 = x
                | n >  0 = index2 xs (n-1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 x (y:ys) | x == y = True
               | otherwise = elem2 x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge y [] = y
merge (x:xs) (y:ys) | x >= y = y : merge (x:xs) ys
                    | x <  y = x : merge xs     (y:ys)

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve x = ( take center x, drop center x)
  where center = div (length x) 2


msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort x = merge (msort m) (msort n)
  where
    (m, n) = halve x

sumA :: Num a => [a] -> a
sumA []      = 0
sumA (x:xs)  = x + sumA xs

takeB :: Int -> [a] -> [a]
takeB 0 _  = []
takeB _ [] = []
takeB n (x:xs)  = x:(takeB (n-1) xs)

lastC :: [a] -> a
lastC [x] = x
lastC (x:xs) = lastC xs





