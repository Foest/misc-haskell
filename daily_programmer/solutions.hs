import Control.Monad
import Data.List
import Data.Matrix
--Solutions to DailyProgrammer problems
--------------------------------------------------------------------------------

dp_217e d h
  | h > d = (1/d) * (dp_217e d (h - d))
  | otherwise = (d-h+1) / d

--------------------------------------------------------------------------------

dp_265e n 'p' w = last . (take n) . sort . permutations $ [0..w-1]
dp_265e' n 'c' v w = last . take (n) $ helper v [0..w-1]

helper 0 _ = [[]]
helper n ys = do
  (x:xs) <- tails ys
  rest <- helper (n-1) xs
  return $ x : rest

--------------------------------------------------------------------------------

dp_270h = do
  putStrLn "Enter a number"
  n <- getLine
  putStrLn "Enter rows separated by returns"
  lines <- replicateM (read n) (getLine)
  mat <- return $ fromLists lines
  largestBox <- return $ findLargestBox mat
  putStrLn $ "We can deploy " ++ (show (largestBox * largestBox)) ++ " dropships!"

findLargestBox mat = maximum . map matMap $ coords
  where coords = [(x,y) | x <- [1..(ncols mat)], y <- [1..(nrows mat)]]
        matMap (x,y) = calculateLargestBox y y x x mat

calculateLargestBox sr er sc ec mat
  | sr > nrows mat || er > nrows mat || sc > ncols mat || ec > ncols mat = er - sr
  | elem 'x' (toList (submatrix sr er sc ec mat)) = er - sr
  | otherwise = calculateLargestBox sr (er+1) sc (ec+1) mat

--------------------------------------------------------------------------------
