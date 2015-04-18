module Hw3 where

	-- factorial function, tail recursion
	factorial :: Integer -> Integer
	-- interp. factorial function

	factorial 0 = 1
	factorial n = let go k x = 
						if k > 1 then 
							go (k - 1) (x * k)
						else
							x
				  in go n 1

	-- fibonacci function, tail recursion
	fibonacci :: Integer -> Integer
	-- interp. fibonacci function

	fibonacci 0 = 1
	fibonacci 1 = 1
	fibonacci n = let go n k a b = if n == k then
									b 
								   else if k == (n - 1) then
								 	a
								   else
								   	go n (k + 1) b (a + b)
				  in go n (-1) 0 1  

	-- Newton's binom function, tail recursion
	binom :: Double -> Double -> Integer -> Double
	binom a b n = let go a b n k s = if (k == n) then
										s + b^n
									   else
									   	go a b n (k + 1) (s + (bink n k) * a^(n-k) * b^k)
				  in go a b n 0 0.0

	-- binomial coeff.
	bink :: Integer -> Integer -> Double
	bink n k = fromIntegral (factorial n)/ fromIntegral ((factorial k) * (factorial (n - k)))