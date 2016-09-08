find :: [a]-> Int -> Maybe a
find [] y = Nothing
find (x:xs) 0 = Just x
find (x:xs) y = find xs (y-1)



reverse'list :: [a] -> [a]
reverse'list [] = []
reverse'list(x:xs) = reverse'list xs ++ [x]



remove'repeat :: Eq a => [a] -> [a]
remove'repeat [] = []
remove'repeat (x:[]) = [x]
remove'repeat(x:y:xs) = if x==y
	then remove'repeat(y:xs)
	else x:remove'repeat(y:xs)



make_it :: Eq a => [(a,Int)] -> [(a, Int)]
make_it [] = []	
make_it (x:[]) = x:[]
make_it (x:y:xs) = if (fst x==fst y)
	then (fst y,(snd y + 1)):make_it(xs)
	else x:make_it(y:xs)
number'of'repeats :: Eq a => [a]->[(a,Int)]
number'of'repeats [] = []
number'of'repeats (x:xs) = make_it((x, 1):number'of'repeats(xs))



insert'into :: [a]-> Int-> a -> [a]
insert'into x 0 s = s:x
insert'into [] n s = []
insert'into (x:xs) n s = x:insert'into xs (n-1) s 
