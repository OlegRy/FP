module Semestr2 where

	type Name = Char
	data Term = Var Name | Abs Name Type Term | App Term Term | Lit Ground deriving (Show, Eq)

	data Ground = LInt Int | LBool Bool deriving (Show, Eq)

	data Type = TInt | TBool | TArr Type Type deriving (Show, Eq)

	type Env = [(Name, Type)]

	extend :: Env -> (Name, Type) -> Env
	extend env xt = xt : env

	data TypeError = Err String deriving (Show)

	
