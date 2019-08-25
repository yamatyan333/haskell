import Data.Char
import Data.List

type Bit = Int

bit2int :: [Bit] -> Int
bit2int bits = sum [ w*b | (w, b) <- zip weights bits]
  where weights = iterate (*2) 1

bit2int2 :: [Bit] -> Int
bit2int2 = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2 )

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bit2int) . chop8

transmit :: String -> String 
transmit = decode . channel . encode 

channel :: [Bit] -> [Bit] 
channel = id 

votes :: [String] 
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"] 

count :: Eq a => a -> [a] -> Int 
count x = length . filter (== x) 

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)

result :: Ord a => [a] -> [(Int, a)]
result xs = sort [(count l xs, l) | l <- rmdups xs]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]] 
ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/=[])

elim :: Eq a => a -> [[a]] -> [[a]]
elim n = map ( filter (/= n))

rank :: Ord a => [[a]] -> [a] 
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a 
winner' bs = case rank (rmempty bs) of
                  [c] -> c 
                  (c:cs) -> winner' (elim c bs)

 -- Pracice

-- map f filter (p xs)

alla :: (a -> Bool) -> [a] -> Bool
alla f = and . map f

anyb :: (a -> Bool) -> [a] -> Bool
anyb f = or . map f

takewhilec :: (a -> Bool) -> [a] -> [a]
takewhilec f [] = []
takewhilec f (x:xs) | f x = x : takewhilec f xs
                    | otherwise = []

dropwhiled :: (a -> Bool) -> [a] -> [a]
dropwhiled f [] = []
dropwhiled f (x:xs) | f x = xs
                    | otherwise = dropwhiled  f xs

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr ((:) . f) []

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldr (\x xs -> if p x then x:xs else xs) []

dec2int :: [Int] -> Int
-- dec2int  = foldl (\a x -> read (show a ++ show x) ::Int ) 0
dec2int  = foldl (\a x -> a*10 + x ) 0


curry2 :: ( (a, b) -> c) -> (a -> b -> c)
curry2 f = \x y -> f (x, y) 

uncurry2 :: (a -> b -> c) -> ((a, b) -> c)
uncurry2 f = \(x, y) -> f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a-> [b]
unfold p h t x | p x = [] 
               | otherwise = h x : unfold p h t (t x) 

chop82 :: [Bit] -> [[Bit]]
chop82 = unfold (== []) (take 8 ) (drop 8 )

map3 :: (Eq a, Eq b) => (a -> b) -> [a] -> [b]
map3 f = unfold (==[]) (f . head) (drop 1)

iterate2 :: Eq a => (a -> a) -> a -> [a]
iterate2 f = unfold (\_ -> False) id f 


bit2int3 :: [Bit] -> Int
bit2int3 = foldr (\x y -> x + 2*y) 0

int2bin2 :: Int -> [Bit]
int2bin2 0 = []
int2bin2 n = n `mod` 2 : int2bin2 (n `div` 2 )

make82 :: [Bit] -> [Bit]
make82 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity xs | odd (count 1 xs) = 1
          | otherwise = 0

addparity :: [Bit] -> [Bit]
addparity xs = (parity xs) : xs

encode2 :: String -> [Bit]
encode2 = concat . map (addparity . make82 . int2bin2 . ord)

chop83 :: [Bit] -> [[Bit]]
chop83 [] = []
chop83 bits = take 9 bits : chop83 (drop 9 bits)

checkparity :: [Bit] -> [Bit]
checkparity xs | parity (drop 1 xs) == head xs = drop 1 xs
               | otherwise = error "yabaiyo"

decode2 :: [Bit] -> String
decode2 = map (chr . bit2int2 . checkparity) . chop83

transmit2 :: String -> String 
transmit2 = decode2 . channel2 . encode2 

channel2 :: [Bit] -> [Bit] 
channel2 = id 

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = f x : altMap f g []
altMap f g (x1:x2:xs) = f x1 : g x2 : altMap f g xs


luhnDouble :: Int -> Int
luhnDouble x | tmp > 9 = tmp - 9
             | otherwise = tmp
  where tmp = 2 * x

luhnList :: [Int] -> [Int] 
luhnList [] = []
luhnList [x] = luhnDouble x : luhnList []
luhnList (x1:x2:xs) = luhnDouble x1 : x2 : luhnList xs

luhn :: [Int] -> Bool
luhn x = (sum ( luhnList x) ) `mod` 10  == 0
