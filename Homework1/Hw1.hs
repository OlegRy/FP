module Hw1 where

	-- factorial with tail recursion
	factorial :: Integer -> Integer -> Integer
	factorial 0 x = x
	factorial n x = factorial' (n - 1) (x * n)
