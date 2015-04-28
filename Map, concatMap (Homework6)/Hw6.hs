module Hw6 where

	----------------------------------------------------------Task 1-----------------------------------------------------

	-- map through foldr
	mapr :: (a -> b) -> [a] -> [b]
	mapr _ [] = []
	mapr f lst = foldr (\a lst -> (f a):lst) [] lst

	-- map through foldl
	mapl :: (a -> b) -> [a] -> [b]
	mapl _ [] = []
	mapl f lst = foldl (\lst a -> (f a):lst) [] lst

	--------------------------------------------------------Task 2-----------------------------------------------------

	-- concatMap through recursion
	concatMapRec :: (a -> [b]) -> [a] -> [b]
	concatMapRec _ [] = []
	concatMapRec f (x:xs) = (f x) ++ (concatMapRec f xs)

	-- concatMap through foldr
	concatMapr :: (a -> [b]) -> [a] -> [b]
	concatMapr _ [] = []
	concatMapr f lst = foldr (\a b -> (f a) ++ b) [] lst

	-- concatMap through foldl
	concatMapl :: (a -> [b]) -> [a] -> [b]
	concatMapl _ [] = []
	concatMapl f lst = foldl (\a b -> a ++ (f b)) [] lst