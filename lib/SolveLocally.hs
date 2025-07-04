module SolveLocally
	( Solution(Contradiction, Solved, Unsolved)
	, solveGrid
	)
where

import Prelude hiding (MonadFail, either, fail)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy)
import Nonogram
	( Grid, Vertical, Horizontal
	, Hints(rowHints, colHints), widthFromHints, hintOne, fromWidth
	)
import SolveClass
	( CellInfo(..), either
	, MonadFail, fail
	, StateGrid
	, readRowIndices, readColIndices, readGrid, readCol, readRow
	, updateCol, updateRow
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

deduceRow :: StateGrid m => [Int] -> Vertical -> m ()
deduceRow hint index = readRow index >>= deduceLine hint >>= updateRow index

deduceCol :: StateGrid m => [Int] -> Horizontal -> m ()
deduceCol hint index = readCol index >>= deduceLine hint >>= updateCol index

-- Update every row and column in a grid using their hints
stepGrid :: StateGrid m => Hints -> m ()
stepGrid hints = do
	xs <- readColIndices
	ys <- readRowIndices
	traverse_ (uncurry deduceRow) $ zip (rowHints hints) ys
	traverse_ (uncurry deduceCol) $ zip (colHints hints) xs

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

-- Given a proxy of a type used to implement stateful grid updates,
-- and the hints for a problem, solve the grid using local logic.
-- The proxy is needed so that the type m can be attached to a value,
-- for some reason using a type application is not enough
solveGrid :: forall m. (StateGrid m, RunGrid m) => Proxy m -> Hints -> Solution
solveGrid _ hints =
	case
		runOnUnknown @m (solveGridM hints *> readGrid) $
		fromWidth $ widthFromHints hints
	of
		Nothing -> Contradiction
		Just g -> case isSolved g of
			Nothing -> Unsolved g
			Just s -> Solved s