module Hw3 where
	----------------------------------------------------------Task1-------------------------------------------------
	-- Problem A
	type OatmealTemp = Int

	cold, hot, good :: OatmealTemp
	cold = 0
	hot  = 20
	good = 10

	-- Problem B
	data Adjustment = ToRight | ToLeft | Stay deriving (Show, Eq)

	-- Problem C
	advice :: OatmealTemp -> Adjustment
	advice temp
			| temp > good = ToLeft
			| temp < good = ToRight
			| otherwise   = Stay

	---------------------------------------------------------Task2--------------------------------------------------
	-- Problem A
	data DinnerOrder = Chicken | Pasta | Non

	-- Problem B
	dinner_order_to_msg :: DinnerOrder -> String
	dinner_order_to_msg d = case d of
							Chicken -> "Passenger has ordered chicken!"
							Pasta   -> "Passenger has ordered pasta!"
							Non     -> "Passenger has nothing ordered!"
							

