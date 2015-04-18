module Hw5 where
--------------------------------------------------------Problem 1----------------------------------------

	data IntTree = Nill | Val Integer IntTree IntTree deriving (Show, Eq)

	nilTree, leaf, bin_tree :: IntTree
	nilTree = Nill
	leaf = Val 2 Nill Nill
	bin_tree = Val 10 (Val 9 (Val 8 (Val 7 (Val 6 Nill Nill) Nill) (Val 9 Nill Nill)) (Val 11 Nill Nill)) (Val 15 (Val 14 (Val 13 Nill Nill) Nill) Nill)
	height :: IntTree -> Integer
	height t = case t of
				Nill -> 0
				Val i left right -> 1 + (max (height left) (height right))

	sum_int_tree :: IntTree -> Integer
	sum_int_tree t = case t of
						Nill -> 0
						Val i left right -> i + (sum_int_tree left) + (sum_int_tree right)

	search_int_elem :: Integer -> IntTree -> IntTree
	search_int_elem val t = case t of
								Nill -> Nill
								Val i left right -> if (i == val) then
														t
													else if (i > val) then
														search_int_elem val left
													else
														search_int_elem val right

-------------------------------------------------------Problem 2--------------------------------------------

	data BinTree a = BNill | BVal a (BinTree a) (BinTree a) deriving (Show, Eq)

	b_height :: BinTree a -> Integer
	b_height t = case t of
					BNill -> 0
					BVal val left right -> 1 + (max (b_height left) (b_height right))

	tmap :: BinTree a -> (a -> b) -> BinTree b
	tmap t f = case t of
				BNill -> BNill
				BVal val left right -> BVal (f val) (tmap left f) (tmap right f)

	-- tests
	t1 = BVal "I" (BVal "You" (BVal "He" (BVal "She" (BVal "It" BNill BNill) BNill) (BVal "All" BNill BNill)) (BVal "Hello" BNill BNill)) (BVal "World" (BVal "Thanks" (BVal "LOL" BNill BNill) BNill) BNill)

	test_t1 = tmap t1 (\s -> length s)

-----------------------------------------------------Problem 3------------------------------------------
	data AlterList a b = ANill | AVal a (AlterList b a) deriving (Show, Eq)

	list_length :: AlterList a b -> Integer
	list_length list = case list of
						ANill -> 0
						AVal val atail -> 1 + (list_length atail)

	dmap :: AlterList a b -> (a -> c) -> (b -> d) -> AlterList c d
	dmap list f1 f2 = case list of
						ANill -> ANill
						AVal val atail -> AVal (f1 val) (dmap atail f2 f1)

	--tests
	l1 = AVal 5 (AVal "Hello" (AVal 2 (AVal "World" ANill)))
	test_l1 = dmap l1 (\i -> mod i 2 == 0) (\s -> length s)
