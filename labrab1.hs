import Data.Char

sumMy :: Num a => [a] -> a
sumMy [ ] = 0
sumMy (x:xs) = x + sumMy xs

productMy :: Num a => [a] -> a
productMy [ ] = 1
productMy (x:xs) = x * productMy xs

maxMy :: Ord a => a -> a -> a
maxMy x y = if x > y then x else y

minMy :: Ord a => a -> a -> a
minMy x y = if x < y then x else y

maximumMy :: Ord a => [a] -> a
maximumMy [ ] = error "пустой список"
maximumMy [x] = x
maximumMy (x:xs) = if x > maximumMy xs then x else maximumMy xs

minimumMy :: Ord a => [a] -> a
minimumMy [ ] = error "пустой список"
minimumMy [x] = x
minimumMy (x:xs) = if x < minimumMy xs then x else minimumMy xs

evenMy :: Integral a => a -> Bool
evenMy x = if mod x 2 == 0 then True else False

oddMy :: Integral a => a -> Bool
oddMy x = if mod x 2 == 1 then True else False

gcdMy :: Integral a => a -> a -> a
gcdMy x y = if x == y then x 
			else if x > y then gcdMy (x - y) y
			 	 else gcdMy x (y - x)

lcmMy :: Integral a => a -> a -> a
lcmMy x y = div (x * y) (gcdMy x y)

expMy :: (Num a, Integral b) => a -> b -> a
expMy _ 0 = 1
expMy x y = x * expMy x (y - 1)



factMy :: Integer -> Integer
factMy 0 = 1
factMy x = x * factMy (x - 1)

fibMy :: Integer -> Integer
fibMy 1 = 0
fibMy 2 = 1
fibMy x = fibMy (x-1) + fibMy (x-2)

fibMy' :: Integer -> Integer
fibMy' n = fiboMy' n 0 1
fiboMy' 0 prev prevprev = prevprev
fiboMy' n prev prevprev = fiboMy' (n - 1) (prev + prevprev) prev


andMy :: [Bool] -> Bool
andMy[] = True
andMy(x:xs) = if x == False then False else andMy xs

orMy :: [Bool] -> Bool
orMy[] = False
orMy(x:xs) = if x == True then True else orMy xs



headMy :: [a] -> a
headMy (x:xs) = x

tailMy :: [a] -> [a]
tailMy (x:xs) = xs

lastMy :: [a] -> a
lastMy [x] = x
lastMy (x:xs) = lastMy xs

initMy :: [a] -> [a]
initMy [_] = []
initMy (x:xs) = x : initMy xs

lengthMy :: [a] -> Int
lengthMy [] = 0
lengthMy (x:xs) = 1 + lengthMy xs

indexMy :: [a] -> Int -> a
indexMy (x:xs) y = if y > 0 then indexMy xs (y - 1) else x

gluingMy:: [a] -> [a] -> [a]
gluingMy [] x = x
gluingMy (x:xs) y = x : gluingMy xs y

concatMy :: [[a]] -> [a]
concatMy [x] = x
concatMy (x:xs) = gluingMy x (concatMy xs)

takeMy :: Int -> [a] -> [a]
takeMy 0 y = []
takeMy x (y:ys) = if x >= lengthMy (y:ys) then (y:ys) else y : takeMy (x - 1) ys

dropMy :: Int -> [a] -> [a]
dropMy x (y:ys) = if x >= lengthMy (y:ys) then [] 
				  else if x > 0 then dropMy (x - 1) ys 
					   else (y:ys)

reverseMy :: [a] -> [a]
reverseMy xs = reverseMy' xs []
reverseMy' [] acc = acc
reverseMy' (x:xs) acc = reverseMy' xs (x:acc)

elemMy :: Eq a => a -> [a] -> Bool
elemMy x [] = False
elemMy x (y:ys) = if x == y then True else elemMy x ys

replicateMy :: Int -> a -> [a]
replicateMy 0 _ = []
replicateMy x y = y : replicateMy (x - 1) y



lookupMy :: Eq a => a -> b -> [(a, b)] -> b
lookupMy _ xb [] = xb
lookupMy xa xb (a:b) = if fst a == xa then snd a else lookupMy xa xb b

substrMy :: [a] -> Int -> Int -> [a]
substrMy a x y = take (y - x + 1) (drop (x - 1) a)

strReplaceMy :: Eq a => [a] -> [a] -> [a] -> [a]
strReplaceMy _ _ [] = []
strReplaceMy a b (c:cs) = if a == take (length a) (c:cs) then b ++ strReplaceMy a b (drop (length a) (c:cs)) else c : strReplaceMy a b cs

elemIndeciesMy :: Eq a => a -> [a] -> [Int]
elemIndeciesMy x ys = elemIndeciesMy' x ys [] 0
elemIndeciesMy' x [] z n = reverse z
elemIndeciesMy' x (y:ys) z n = if x == y then elemIndeciesMy' x ys (n : z) (n + 1) else elemIndeciesMy' x ys z (n + 1)

strPosMy :: Eq a => [a] -> [a] -> [Int]
strPosMy xs ys = strPosMy' xs ys [] 0
strPosMy' xs [] z n = reverse z
strPosMy' xs (y:ys) z n = if xs == take (length xs) (y:ys) then strPosMy' xs ys (n : z) (n + 1) else strPosMy' xs ys z (n + 1)

strRotateMy :: [a] -> Int -> [a]
strRotateMy [] _ = []
strRotateMy xs y = if y > 0 then strRotateMy (last xs : init xs) (y - 1) else xs

unevenHandWritingMy :: String -> String
unevenHandWritingMy [] = []
unevenHandWritingMy (x:y:z:n) = if isLower z == True then x : y : toUpper z : unevenHandWritingMy n else x : y : toLower z : unevenHandWritingMy n
unevenHandWritingMy (x) = (x)
unevenHandWritingMy (x:y) = (x:y)
