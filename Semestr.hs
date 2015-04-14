module Semestr where

	type Name = Char
	data Term = Var Name | Abs Name Term | App Term Term deriving (Show, Eq)

	-- term substituation function
	subs :: Term -> Term -> Name -> Term
	-- interp. of substituation

	subs t (Var y) x = if x == y then t else Var y
	subs t (Abs y t1) x = if x == y then
							Abs y t1
					   else
					   		Abs y (subs t t1 x)
	subs t (App t1 t2) x = App (subs t t1 x) (subs t t2 x)

	-- term eval function
	eval :: Term -> Term
	-- interp. of eval function

	eval (Var s)     = Var s
	eval (Abs s t)   = Abs s (eval t)
	eval (App t1 t2) =  case t1 of
						Var x -> Var x
						Abs x t -> eval(subs t2 t x)
						App (Var x) t -> App t1 t2
						App t3 t4 -> eval (App (eval t1) t2)

	-- test lambdas
	id'  = Abs 'x' (Var 'x') -- identify function
	id_test = eval (App id' (App id' (Abs 'z' (App id' (Var 'z')))))

	tru = Abs 't' (Abs 'f' (Var 't')) -- Church bool true constant
	fls = Abs 't' (Abs 'f' (Var 'f')) -- Church bool false constant
	tru_test = eval(App (App (App (Abs 'l' (Abs 'm' (Abs 'n' (App (App (Var 'l') (Var 'm')) (Var 'n'))))) tru) (Var 'v')) (Var 'w'))
