module Semestr2 where

	type Name = Char

	-- define context type
	type Context = [(Name, Type)]

	-- type for lambda term
	data Type = TInt | TBool | TArr Type Type | TErr deriving (Show, Eq)

	-- define term data type
	data Term = Var Name | Abs Name Type Term | App Term Term | Lit Ground deriving (Show, Eq)

	data Ground = LInt | LBool deriving (Show, Eq)

	-- add elem to (Name, Type) to Context
	extend :: Context -> (Name, Type) -> Context
	extend c xt = xt:c

	checkTerm :: Context -> Term -> Type
	-- interp. checking type of term

	-- we have literal
	checkTerm _ (Lit a) = case a of
						LInt -> TInt
						LBool -> TBool

	-- we have variable
	checkTerm c (Var x) = case (lookup x c) of
							Just e -> e
							Nothing ->  TErr

	-- we have abstraction (lambda function)
	checkTerm c (Abs n ty te) = case ty of
									TErr -> TErr
									_ -> TArr ty (checkTerm (extend c (n, ty)) te)

	-- we have application
	checkTerm c (App t1 t2) = if ((checkTerm c t1) == TErr || (checkTerm c t2) == TErr)
								then TErr
							  else
								checkTArr (checkTerm c t1) (checkTerm c t2)

	-- check TArr from application
	checkTArr :: Type -> Type -> Type
	checkTArr _ _ = TErr
	checkTArr (TArr t1 t2) t = if t1 == t2 then t1 else TErr

	-- check valid type of term
	check :: Type -> Bool
	check t = case t of
				TErr -> False
				_ -> True


	
