quicksort :: [Int] -> [Int]
quicksort []     = []
quicksort (p:xs) = (quicksort lessThan) ++ [p] ++ (quicksort greaterThan)
    where
        lessThan  = filter (< p) xs
        greaterThan = filter (>= p) xs

pegaPosicao :: Int -> [Int] -> Int
pegaPosicao 0 xs = head xs
pegaPosicao p xs
    | p > 0 = pegaPosicao (p - 1) (tail xs)
    | otherwise = error "Posicao invalida"

pega :: Int -> [Int] -> [Int]
pega 0 _  = []
pega _ [] = []
pega n xs = head xs:pega (n - 1) (tail xs)

retira :: Int -> [Int] -> [Int]
retira 0 xs = xs
retira _ [] = [] 
retira n xs = retira (n - 1) (tail xs)

mediaLista :: [Int] -> Float
mediaLista xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)

pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores n xs = pega n ((reverse . quicksort) xs)

contaMaiores :: Int -> [Int] -> Int
contaMaiores n [] = 0
contaMaiores n xs = (if n < head xs then 1 else 0) + (contaMaiores n) (tail xs) 

intercala :: [a] -> [a] -> [a]
intercala l1 [] = l1
intercala l1 l2 = f l1 l2 []
    where f l1 l2 result 
            | null l1 = result ++ l2
            | null l2 = result ++ l1
            | otherwise = f (tail l1) (tail l2) (result ++ [head l1] ++ [head l2])  

dupli :: [a] -> [a]
dupli xs = intercala xs xs

repli :: Int -> [a] -> [a]
repli n xs = foldl (++) [] elems
    where elems = map (((take n) . repeat)) xs   

drobEvery :: Int -> [a] -> [a]
drobEvery n as 
    | length as < n = as
    | otherwise = take n as ++ drobEvery n (drop (n + 1) as) 

split :: Int -> [a] -> ([a], [a])
split n as = (take n as, drop n as)

