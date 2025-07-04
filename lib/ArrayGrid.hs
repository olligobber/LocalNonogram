module ArrayGrid
	( ArrayGrid )
where

import Prelude hiding (MonadFail, fail)
import Data.Traversable (for)
import Data.Foldable (traverse_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Nonogram
	( Grid(Grid), Height, Width, Vertical, Horizontal
	, firstHorizontal, firstVertical, lastHorizontal, lastVertical
	)
import SolveClass
	( MonadFail(fail)
	, both
	, CellInfo(Unknown)
	, ReadGrid(readGrid, readWidth, readHeight, readRow, readCol)
	, readRowIndices, readColIndices
	, WriteGrid(updateRow, updateCol)
	, RunGrid(runOnUnknown)
	)

data GridInfo s = GridInfo
	{ gridHeight :: Height
	, gridWidth :: Width
	, arrayPointer :: STArray s (Horizontal, Vertical) CellInfo
	}

newtype ArrayGrid x = ArrayGrid {
	runGrid :: forall s. GridInfo s -> ST s (Maybe x)
	}

instance Functor ArrayGrid where
	fmap f (ArrayGrid a) = ArrayGrid $ fmap (fmap $ fmap f) a

instance Applicative ArrayGrid where
	pure x = ArrayGrid $ pure $ pure $ pure x
	a <*> b = ArrayGrid $ \i -> do -- ST Monad
		runA <- runGrid a i
		case runA of
			Nothing -> pure Nothing
			Just f -> fmap f <$> runGrid b i

instance Monad ArrayGrid where
	a >>= f = ArrayGrid $ \i -> do -- ST Monad
		runA <- runGrid a i
		case runA of
			Nothing -> pure Nothing
			Just x -> runGrid (f x) i

instance MonadFail ArrayGrid where
	fail = ArrayGrid $ pure $ pure $ Nothing

getCell :: Horizontal -> Vertical -> ArrayGrid CellInfo
getCell x y = ArrayGrid $ \i -> Just <$> readArray (arrayPointer i) (x,y)

writeCell :: Horizontal -> Vertical -> CellInfo -> ArrayGrid ()
writeCell x y val =
	ArrayGrid $ \i -> Just <$> writeArray (arrayPointer i) (x,y) val

modifyCell ::
	Horizontal -> Vertical -> (CellInfo -> ArrayGrid CellInfo) -> ArrayGrid ()
modifyCell x y f = do
	oldVal <- getCell x y
	newVal <- f oldVal
	writeCell x y newVal

instance ReadGrid ArrayGrid where
	readGrid = do
		xs <- readColIndices
		ys <- readRowIndices
		Grid <$> for xs (\x -> for ys $ \y -> getCell x y)

	readWidth = ArrayGrid $ \i -> pure $ pure $ gridWidth i

	readHeight = ArrayGrid $ \i -> pure $ pure $ gridHeight i

	readRow y = do
		xs <- readColIndices
		for xs $ \x -> getCell x y

	readCol x = do
		ys <- readRowIndices
		for ys $ \y -> getCell x y

instance WriteGrid ArrayGrid where
	updateRow y vals = do
		xs <- readColIndices
		traverse_ (\(x, val) -> modifyCell x y (both val)) $ zip xs vals

	updateCol x vals = do
		ys <- readRowIndices
		traverse_ (\(y, val) -> modifyCell x y (both val)) $ zip ys vals

instance RunGrid ArrayGrid where
	runOnUnknown a w h = runST $ do -- ST Monad
		p <- newArray
			(
				(firstHorizontal, firstVertical),
				(lastHorizontal w, lastVertical h)
			)
			Unknown
		runGrid a $ GridInfo
			{ gridHeight = h
			, gridWidth = w
			, arrayPointer = p
			}

