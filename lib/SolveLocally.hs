module SolveLocally
	( Solution(Contradiction, Solved, Unsolved)
	, solveGrid
	)
where

import Prelude hiding (MonadFail, either, fail)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy)
import Nonogram
	(Grid, Hints(rowHints, colHints), sizeFromHints, parseHints, hintOne)
import SolveClass
	( CellInfo(..), either
	, MonadFail, fail
	, ReadGrid, WriteGrid, StateGrid
	, readGrid, readCol, readRow, updateCol, updateRow
	, RunGrid, runOnUnknown
	)

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

data Solution
	= Contradiction
	| Solved (Grid Bool)
	| Unsolved (Grid CellInfo)

isSolved :: Grid CellInfo -> Maybe (Grid Bool)
isSolved = traverse $ \cell -> case cell of
	Full -> Just True
	Empty -> Just False
	Unknown -> Nothing

solveGrid :: forall m. (StateGrid m, RunGrid m) => Proxy m -> Hints -> Solution
solveGrid _ hints =
	case
		runOnUnknown @m (solveGridM @m hints *> (readGrid @m)) $
		sizeFromHints hints
	of
		Nothing -> Contradiction
		Just g -> case isSolved g of
			Nothing -> Unsolved g
			Just s -> Solved s