module ArrayGrid
	( ArrayGrid )
where

import Prelude hiding (MonadFail, fail)
import Data.Traversable (for)
import Data.Foldable (traverse_)
import Data.Ix (range)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray)
import Data.Array.MArray (newArray, readArray, writeArray, getBounds)
import Nonogram
	(Grid(Grid), Vertical(Vertical), Horizontal(Horizontal, fromHorizontal))
import SolveClass
	( MonadFail(fail)
	, both
	, CellInfo(Unknown)
	, ReadGrid(readGrid, readSize, readRow, readCol)
	, WriteGrid(updateRow, updateCol)
	, RunGrid(runOnUnknown)
	)

type GridPointers s = STArray s (Horizontal, Vertical) CellInfo

newtype ArrayGrid x = ArrayGrid {
	runGrid :: forall s. GridPointers s -> ST s (Maybe x)
	}

instance Functor ArrayGrid where
	fmap f (ArrayGrid a) = ArrayGrid $ fmap (fmap $ fmap f) a

instance Applicative ArrayGrid where
	pure x = ArrayGrid $ pure $ pure $ pure x
	a <*> b = ArrayGrid $ \p -> do -- ST Monad
		runA <- runGrid a p
		case runA of
			Nothing -> pure Nothing
			Just f -> fmap f <$> runGrid b p

instance Monad ArrayGrid where
	a >>= f = ArrayGrid $ \p -> do -- ST Monad
		runA <- runGrid a p
		case runA of
			Nothing -> pure Nothing
			Just x -> runGrid (f x) p

instance MonadFail ArrayGrid where
	fail = ArrayGrid $ pure $ pure $ Nothing

getCell :: Horizontal -> Vertical -> ArrayGrid CellInfo
getCell x y = ArrayGrid $ \p -> Just <$> readArray p (x,y)

writeCell :: Horizontal -> Vertical -> CellInfo -> ArrayGrid ()
writeCell x y val = ArrayGrid $ \p -> Just <$> writeArray p (x,y) val

modifyCell ::
	Horizontal -> Vertical -> (CellInfo -> ArrayGrid CellInfo) -> ArrayGrid ()
modifyCell x y f = do
	oldVal <- getCell x y
	newVal <- f oldVal
	writeCell x y newVal

instance ReadGrid ArrayGrid where
	readGrid = do
		n <- readSize
		Grid <$> for (range (Horizontal 0, Horizontal $ n-1)) ( \x ->
			for (range (Vertical 0, Vertical $ n-1)) $ \y -> getCell x y)
	readSize = ArrayGrid $ \p -> do
		(_, (n, _)) <- getBounds p
		pure $ pure $ fromHorizontal n + 1
	readRow y = do
		n <- readSize
		for (range (Horizontal 0, Horizontal $ n-1)) (\x -> getCell x y)
	readCol x = do
		n <- readSize
		for (range (Vertical 0, Vertical $ n-1)) (\y -> getCell x y)

instance WriteGrid ArrayGrid where
	updateRow y vals = do
		n <- readSize
		traverse_ (\(x, val) -> modifyCell x y (both val)) $
			zip (range (Horizontal 0, Horizontal $ n-1)) vals
	updateCol x vals = do
		n <- readSize
		traverse_ (\(y, val) -> modifyCell x y (both val)) $
			zip (range (Vertical 0, Vertical $ n-1)) vals

instance RunGrid ArrayGrid where
	runOnUnknown a n = runST $ do -- ST Monad
		p <- newArray
			((Horizontal 0, Vertical 0), (Horizontal $ n-1, Vertical $ n-1))
			Unknown
		runGrid a p

