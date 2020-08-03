aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes f = f . f

vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal f n = foldl (+) 0 (map f [0..n])

foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt _ []     = 0
foldInt _ (x:[]) = x
foldInt f (x:xs) = f x (foldInt f xs)

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt _ [] = []
mapInt f (x:xs) = f x : mapInt f xs

somaQuadrado :: [Int] -> Int
somaQuadrado = foldInt (+) . mapInt (\x -> x * x)

filterString :: (Char -> Bool) -> [Char]-> [Char]
filterString _ [] = []
filterString f (c:cs) 
    | f c       = c:filterString f cs
    | otherwise = filterString f cs

iter :: Int -> (Int -> Int) -> Int -> Int
iter 0 _ x = x 
iter n f x = f $ iter (n - 1) f x

 