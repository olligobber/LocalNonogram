-- Given a hint on each line, output on each line
	-- 0 if it is contradictory (no solution)
	-- 1 if it is logically solvable (one solution)
	-- 2 if it is not logically solvable (no idea how many solutions)

import Data.Proxy
import Nonogram (Hints, parseHints)
import ArrayGrid (ArrayGrid)
import SolveLocally (Solution(..), solveGrid)

solveAndOutput :: Hints -> Int
solveAndOutput hints = case solveGrid (Proxy @ArrayGrid) hints of
	Contradiction -> 0
	Solved _ -> 1
	Unsolved _ -> 2

main :: IO ()
main = interact $ unlines . fmap (show . solveAndOutput . parseHints) . lines