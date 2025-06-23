module SolveTools
	( MonadFail(fail)
	, Deduction(either, both)
	, CellInfo(Full, Empty, Unknown)
	, ReadGrid(readGrid, readSize, readRow, readCol)
	, WriteGrid(updateRow, updateCol)
	, StateGrid
	, SimpleGrid(SimpleGrid, runGrid)
	, runOnBlank
	)
where

import Prelude hiding (MonadFail, either, fail)
import Nonogram (Grid(Grid, getRows), sizeFromGrid, getRow, getCol)

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