import Data.List

{-
Solution to Daily Programmer Challenge 115 Intermediate
By H. F. Williams
01/12/2013
-}

--sumPairs returns a list of pairs, each of which sums to the target number
sumPairs :: (Num t, Ord t) => [t] -> t -> [(t,t)] 
sumPairs numbers target = func (qSort numbers) target

func :: (Num t, Ord t) => [t] -> t -> [(t,t)] 
func [] _ = []
func nums targ
	| front + back > targ = func (init nums) targ
	| front + back < targ = func (tail nums) targ
	| front + back == targ = (front,back) : func (tail nums) targ
	where front = head nums
	      back = last nums  

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = (qSort smaller) ++ [x] ++ (qSort bigger)
	where smaller = filter (<x) xs
	      bigger = filter (>=x) xs
