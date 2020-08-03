
takeWhileEqual :: Eq a => a -> [a] -> [a]
takeWhileEqual x xs = takeWhile (== x) xs

cat :: [Char] -> [[Char]]
cat [] = []
cat cs =  equals : cat remaining 
    where 
    	first = head cs
	equals = takeWhileEqual first cs
	remaining = drop (length equals) cs
