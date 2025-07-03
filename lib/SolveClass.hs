module SolveClass
	( MonadFail(fail)
	, Deduction(either, both)
	, CellInfo(Full, Empty, Unknown)
	, ReadGrid(readGrid, readSize, readRow, readCol)
	, WriteGrid(updateRow, updateCol)
	, RunGrid(runOnUnknown)
	, StateGrid
	)
where

import Prelude hiding (MonadFail, either, fail)
import Nonogram (Grid(Grid), sizeFromGrid, getRow, getCol)

-- Monad with support for simple failure
-- `fail >>= x = fail`
-- `x *> fail = fail`
class Monad m => MonadFail m where
	fail :: m x

instance MonadFail Maybe where
	fail = Nothing

-- A class representing deductions which can be combined in two ways
class Deduction x where
	-- either creates a more general deduction which both inputs imply
	either :: x -> x -> x
	-- both creates a more specific deduction which implies both inputs
	both :: MonadFail m => x -> x -> m x

-- A type representing knowledge about a cell in a nonogram grid
-- A cell is either known to be full, known to be empty, or unknown
data CellInfo = Full | Empty | Unknown deriving (Eq)

instance Deduction CellInfo where
	either a b
		| a == b = a
		| otherwise = Unknown

	both Unknown b = pure b
	both a Unknown = pure a
	both a b
		| a == b = pure a
		| otherwise = fail

-- Deductions apply to lists by being applied componentwise
instance Deduction x => Deduction [x] where
	either = zipWith either
	both as bs = traverse (uncurry both) $ zip as bs

-- Deductions apply to grids by being applied componentwise
instance Deduction x => Deduction (Grid x) where
	either (Grid g) (Grid h) = Grid $ either g h
	both (Grid g) (Grid h) = Grid <$> both g h

-- A class for reading the current state of deductions about the grid
class Monad m => ReadGrid m where
	readGrid :: m (Grid CellInfo)
	readSize :: m Int
	readSize = sizeFromGrid <$> readGrid
	readRow :: Int -> m [CellInfo]
	readRow n = getRow n <$> readGrid
	readCol :: Int -> m [CellInfo]
	readCol n = getCol n <$> readGrid

-- A class for updating knowledge about the grid
-- Updates use `both` from the `Deduction` class to merge new info into
-- existing info, possibly failing
class MonadFail m => WriteGrid m where
	updateRow :: Int -> [CellInfo] -> m ()
	updateCol :: Int -> [CellInfo] -> m ()

-- A class for running a grid solver on a square grid that starts all unknown
class RunGrid m where
	runOnUnknown :: m x -> Int -> Maybe x

-- Both Read and Write in one convenient predicate
type StateGrid m = (ReadGrid m, WriteGrid m)