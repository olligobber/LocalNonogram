module SimpleGrid
	( SimpleGrid )
where

import Prelude hiding (MonadFail, fail)
import Nonogram (Grid(Grid, getRows), fromVertical, fromHorizontal, getRow, getCol)
import SolveClass
	( MonadFail(fail)
	, both
	, CellInfo(Unknown)
	, ReadGrid(readGrid)
	, WriteGrid(updateRow, updateCol)
	, RunGrid(runOnUnknown)
	)

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
			newRows = replaceIndex (fromVertical index) oldRows newRow
			newGrid = Grid newRows
		pure (newGrid, ())
	updateCol index col = SimpleGrid $ \g -> do -- Maybe Monad
		newCol <- both (getCol index g) col
		let
			oldRows = getRows g
			newRows =
				zipWith (replaceIndex $ fromHorizontal index) oldRows newCol
			newGrid = Grid newRows
		pure (newGrid, ())

instance RunGrid SimpleGrid where
	runOnUnknown s n =
		fmap snd $
		runGrid s $
		Grid $ replicate n $ replicate n $ Unknown