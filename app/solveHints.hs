-- Given a hint on each line, output on each line
	-- 0 if it is contradictory (no solution)
	-- 1 if it is logically solvable (one solution)
	-- 2 if it is not logically solvable (no idea how many solutions)

import Prelude hiding (MonadFail, either, fail)
import Data.Foldable (traverse_)
import Nonogram
	(Grid, Hints(rowHints, colHints), sizeFromHints, parseHints, hintOne)
import SolveClass
	( CellInfo(..), either
	, MonadFail, fail
	, StateGrid, readGrid, readCol, readRow, updateCol, updateRow
	)
import ArrayGrid (runOnBlank)

-- Given a line with partial information, get all lines with full information
possibleLines :: [CellInfo] -> [[Bool]]
possibleLines = traverse $ \x -> case x of
	Unknown -> [True, False]
	Full -> [True]
	Empty -> [False]

-- Given current state of a line and its hints, deduce as much as possible
-- This is done by getting every possible full info version of the line,
-- filtering those that match the hint, and combining the results
deduceLine :: MonadFail m => [Int] -> [CellInfo] -> m [CellInfo]
deduceLine hint line = case filter ((== hint) . hintOne) $ possibleLines line of
	[] -> fail
	xs -> pure $ foldl1 either $ fmap (\b -> if b then Full else Empty) <$> xs

deduceRow :: StateGrid m => [Int] -> Int -> m ()
deduceRow hint index = readRow index >>= deduceLine hint >>= updateRow index

deduceCol :: StateGrid m => [Int] -> Int -> m ()
deduceCol hint index = readCol index >>= deduceLine hint >>= updateCol index

-- Update every row and column in a grid using their hints
stepGrid :: StateGrid m => Hints -> m ()
stepGrid hints = do
	traverse_ (uncurry deduceRow) $ zip (rowHints hints) [0..]
	traverse_ (uncurry deduceCol) $ zip (colHints hints) [0..]

-- Solve a grid
solveGridM :: StateGrid m => Hints -> m ()
solveGridM hints = do
	oldGrid <- readGrid
	stepGrid hints
	newGrid <- readGrid
	if oldGrid == newGrid then
		pure ()
	else
		solveGridM hints

isSolved :: Grid CellInfo -> Bool
isSolved = all (/= Unknown)

-- Solve a grid, outputting either
	-- 0 if it is contradictory (no solution)
	-- 1 if it is logically solvable (one solution)
	-- 2 if it is not logically solvable (no idea how many solutions)
solveGrid :: Hints -> Int
solveGrid hints = case runOnBlank (solveGridM hints) $ sizeFromHints hints of
	Nothing -> 0
	Just (g, _) -> if isSolved g then 1 else 2

main :: IO ()
main = interact $ unlines . fmap (show . solveGrid . parseHints) . lines