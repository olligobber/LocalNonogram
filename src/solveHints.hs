-- Given a hint on each line, output on each line
	-- 0 if it is contradictory (no solution)
	-- 1 if it is logically solvable (one solution)
	-- 2 if it is not logically solvable (no idea how many solutions)

import Prelude hiding (MonadFail, either, fail)
import Data.List (elemIndex, (!!))
import Data.Foldable (traverse_)
import Nonogram (
	Grid(Grid, getRows), sizeFromGrid, getRow, getCol,
	Hints(rowHints, colHints), sizeFromHints, parseHints, hintOne
	)

-- Monad with support for simple failure
-- `fail >>= x = fail`
-- `x *> fail = fail`
class Monad m => MonadFail m where
	fail :: m x

instance MonadFail Maybe where
	fail = Nothing

class Deduction x where
	either :: x -> x -> x
	both :: MonadFail m => x -> x -> m x

data CellInfo = Full | Empty | Unknown deriving (Eq)

instance Deduction CellInfo where
	either Unknown _ = Unknown
	either _ Unknown = Unknown
	either a b
		| a == b = a
		| otherwise = Unknown
	both Unknown b = pure b
	both a Unknown = pure a
	both a b
		| a == b = pure a
		| otherwise = fail

instance Deduction x => Deduction [x] where
	either = zipWith either
	both as bs = traverse (uncurry both) $ zip as bs

instance Deduction x => Deduction (Grid x) where
	either (Grid g) (Grid h) = Grid $ either g h
	both (Grid g) (Grid h) = Grid <$> both g h

class Monad m => ReadGrid m where
	readGrid :: m (Grid CellInfo)
	readSize :: m Int
	readSize = sizeFromGrid <$> readGrid
	readRow :: Int -> m [CellInfo]
	readRow n = getRow n <$> readGrid
	readCol :: Int -> m [CellInfo]
	readCol n = getCol n <$> readGrid

class MonadFail m => WriteGrid m where
	-- Updates use `both` from the `Deduction` class to merge new info into
	-- existing info, possibly failing
	updateRow :: Int -> [CellInfo] -> m ()
	updateCol :: Int -> [CellInfo] -> m ()

type StateGrid m = (ReadGrid m, WriteGrid m)

newtype SimpleGrid x = SimpleGrid {
	runGrid :: Grid CellInfo -> Maybe (Grid CellInfo, x)
	}

instance Functor SimpleGrid where
	fmap f (SimpleGrid s) = SimpleGrid $ fmap (fmap $ fmap f) s

instance Applicative SimpleGrid where
	pure x = SimpleGrid $ \g -> Just (g, x)
	s <*> t = SimpleGrid $ \g -> do -- Maybe Monad
		(h, f) <- runGrid s g
		(i, x) <- runGrid t h
		pure (i, f x)

instance Monad SimpleGrid where
	s >>= f = SimpleGrid $ \g -> do -- Maybe Monad
		(h, x) <- runGrid s g
		runGrid (f x) h

instance MonadFail SimpleGrid where
	fail = SimpleGrid $ pure Nothing

instance ReadGrid SimpleGrid where
	readGrid = SimpleGrid $ \g -> Just (g,g)

replaceIndex :: Int -> [a] -> a -> [a]
replaceIndex index xs x =
	take index xs <>
	[x] <>
	drop (index + 1) xs

instance WriteGrid SimpleGrid where
	updateRow index row = SimpleGrid $ \g -> do -- Maybe Monad
		newRow <- both (getRow index g) row
		let
			oldRows = getRows g
			newRows = replaceIndex index oldRows newRow
			newGrid = Grid newRows
		pure (newGrid, ())
	updateCol index col = SimpleGrid $ \g -> do -- Maybe Monad
		newCol <- both (getCol index g) col
		let
			oldRows = getRows g
			newRows = zipWith (replaceIndex index) oldRows newCol
			newGrid = Grid newRows
		pure (newGrid, ())

-- Blank grid with nothing determined
newGrid :: Int -> Grid CellInfo
newGrid size = Grid $ replicate size $ replicate size $ Unknown

runOnBlank :: SimpleGrid x -> Int -> Maybe (Grid CellInfo, x)
runOnBlank s n = runGrid s $ newGrid n

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