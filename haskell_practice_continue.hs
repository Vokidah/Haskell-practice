for'loop :: a->(a->Bool)->(a->a)->(a->IO())->IO ()
for'loop x p f g
	|p x = g x >> for'loop (f x) p f g
	|otherwise = return ()


idk'how'to'name'it :: [(a->b)]->[a]->[[b]]
idk'how'to'name'it f x = map (\y -> map ($y) f )x

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)
foldTree :: (a->a->a) -> Tree a -> a
foldTree f (Leaf x) = x
foldTree f (Branch x y) = f (foldTree f x) (foldTree f y)
