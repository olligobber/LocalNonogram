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
	, Hints(rowHints, colHints), widthFromHints, heightFromHints
	)
import SolveClass
	( CellInfo(..), either
	, MonadFail, fail
	, StateGrid
	, readRowIndices, readColIndices, readGrid, readCol, readRow
	, updateCol, updateRow
	, RunGrid, runOnUnknown
	)

-- Given hints and a line with partial information,
-- get all lines with full information
possibleLines :: [Int] -> [CellInfo] -> [[Bool]]
possibleLines [] [] = pure []
possibleLines [] (Full:_) = fail
possibleLines [] (_:cells) = (False:) <$> possibleLines [] cells
possibleLines _ [] = fail
possibleLines (hint:hints) (Full:cells) =
	placeHere hint hints (Full:cells)
possibleLines (hint:hints) (cell:cells) =
	((False:) <$> possibleLines (hint:hints) cells) <>
	placeHere hint hints (cell:cells)

-- Helper for above that places a segment right here
placeHere :: Int -> [Int] -> [CellInfo] -> [[Bool]]
placeHere 0 [] [] = pure []
placeHere 0 _ [] = fail
placeHere 0 _ (Full:_) = fail
placeHere 0 hints (_:cells) = (False:) <$> possibleLines hints cells
placeHere _ _ (Empty:_) = fail
placeHere _ _ [] = fail
placeHere n hints (_:cells) = (True:) <$> placeHere (n-1) hints cells

-- Given current state of a line and its hints, deduce as much as possible
-- This is done by getting every possible full info version of the line that
-- matches the hint, then combining them all
deduceLine :: MonadFail m => [Int] -> [CellInfo] -> m [CellInfo]
deduceLine hint line = case possibleLines hint line of
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
		runOnUnknown @m
			(solveGridM hints *> readGrid)
			(widthFromHints hints)
			(heightFromHints hints)
	of
		Nothing -> Contradiction
		Just g -> case isSolved g of
			Nothing -> Unsolved g
			Just s -> Solved s