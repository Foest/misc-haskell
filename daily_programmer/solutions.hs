import Control.Monad
import Data.List
--Solutions to DailyProgrammer problems
dp_217e d h
  | h > d = (1/d) * (dp_217e d (h - d))
  | otherwise = (d-h+1) / d

dp_265e n 'p' w = last . (take n) . sort . permutations $ [0..w-1]
dp_265e' n 'c' v w = last . take (n) $ helper v [0..w-1]

helper 0 _ = [[]]
helper n ys = do
  (x:xs) <- tails ys
  rest <- helper (n-1) xs
  return $ x : rest


dp_270h = do
  putStrLn "Enter a number"
  n <- getLine
  putStrLn "Enter rows separated by returns"
  lines <- replicateM (read n) (getLine)
  sequence_ (map putStrLn lines)
