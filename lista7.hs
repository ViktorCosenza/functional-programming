somaQuadrupla :: [(Int,Int,Int,Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla ((x,y,z,w):xs) = sum [x,y,z,w] + somaQuadrupla xs 

somaTuplas :: [((Int,Int),(Int,Int))] -> Int
somaTuplas [] = 0
somaTuplas (((x, y), (w, z)):xs) = sum [x,y,w,z] + somaTuplas xs 

zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (w:ws) = (x, w):(zipp xs ws) 

zippTres :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
zippTres [] _ _ = []
zippTres _ [] _ = []
zippTres _ _ [] = []
zippTres (x:xs) (w:ws) (z:zs) = (x, w, z):(zippTres xs ws zs) 

unZipp :: [(Int, Int)] -> ([Int], [Int])
unZipp [] = ([], [])
unZipp xs = (unzipEsq xs, unzipDir xs)
    where 
        unzipEsq ((x,_):xs) = x:unzipEsq xs
        unzipEsq [] = []   
        unzipDir ((_,x):xs) = x:unzipDir xs
        unzipDir [] = []   