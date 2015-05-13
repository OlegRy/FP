module Hw2 where

	-- factorial function
	factorial :: Integer -> Integer
	-- interp. factorial function

	factorial 0 = 1
	factorial n = n * factorial (n - 1)

	--fibonacci function
	fibonacci :: Integer -> Integer
	-- interp. fibonacci function

	fibonacci 0 = 1
	fibonacci 1 = 1
	fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


	-- akkerman function
	akkerman ::  Integer -> Integer -> Integer
	-- interp. akkerman function

	akkerman 0 n = n + 1
	akkerman m n = if m > 0 then
					if n == 0 then
						akkerman (m - 1) 1
					else if n > 0 then
						akkerman (m - 1) (akkerman m (n - 1))
					else
						error "n < 0"
				   else
				   	-error "m < 0"