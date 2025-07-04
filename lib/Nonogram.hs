module Nonogram
	( Grid(Grid, getRows)
	, Vertical(Vertical, fromVertical)
	, Horizontal(Horizontal, fromHorizontal)
	, Width(Width, fromWidth)
	, Height(Height, fromHeight)
	, firstHorizontal
	, firstVertical
	, lastHorizontal
	, lastVertical
	, rowIndices
	, colIndices
	, heightFromGrid
	, widthFromGrid
	, getRow
	, getCols
	, getCol
	, storeGrid
	, parseGrid
	, Hints(Hints, colHints, rowHints)
	, heightFromHints
	, widthFromHints
	, storeHints
	, parseHints
	, hintOne
	, hintGrid
	)
where

import Data.List (transpose, intercalate, group)
import Data.Ix (Ix, range)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim l = fst $ go (delim:l) where
	go [] = ([], [])
	go (x:xs)
		| x == delim = (uncurry (flip (:)) $ go xs, [])
		| otherwise = (x:) <$> go xs

newtype Grid x =
	Grid { getRows :: [[x]] }
	deriving (Eq, Ord, Show)

instance Functor Grid where
	fmap f (Grid g) = Grid $ fmap (fmap f) g

instance Foldable Grid where
	foldMap f (Grid g) = foldMap (foldMap f) g

instance Traversable Grid where
	sequenceA (Grid g) = Grid <$> traverse sequenceA g

-- Coordinate systems so we don't mix up horizontal and vertical indices

newtype Vertical = Vertical { fromVertical :: Int } deriving (Eq, Ord, Ix)

newtype Horizontal = Horizontal { fromHorizontal :: Int } deriving (Eq, Ord, Ix)

newtype Height = Height { fromHeight :: Int }

newtype Width = Width { fromWidth :: Int }

firstVertical :: Vertical
firstVertical = Vertical 0

firstHorizontal :: Horizontal
firstHorizontal = Horizontal 0

lastVertical :: Height -> Vertical
lastVertical (Height n) = Vertical $ n - 1

lastHorizontal :: Width -> Horizontal
lastHorizontal (Width n) = Horizontal $ n - 1

rowIndices :: Height -> [Vertical]
rowIndices h = range (firstVertical, lastVertical h)

colIndices :: Width -> [Horizontal]
colIndices w = range (firstHorizontal, lastHorizontal w)

widthFromGrid :: Grid x -> Width
widthFromGrid (Grid []) = Width 0
widthFromGrid (Grid (x:_)) = Width $ length x

heightFromGrid :: Grid x -> Height
heightFromGrid = Height . length . getRows

getRow :: Vertical -> Grid x -> [x]
getRow n = (!! fromVertical n) . getRows

getCols :: Grid x -> [[x]]
getCols = transpose . getRows

getCol :: Horizontal -> Grid x -> [x]
getCol n = fmap (!! fromHorizontal n) . getRows

-- Store in a compact format
storeGrid :: Grid Bool -> String
storeGrid =
	intercalate "2" .
	fmap (fmap $ \b -> if b then '1' else '0') .
	getRows

-- Read the stored grid
parseGrid :: String -> Grid Bool
parseGrid =
	Grid .
	fmap (fmap (=='1')) .
	splitOn '2'

data Hints =
	Hints { rowHints :: [[Int]], colHints :: [[Int]] }
	deriving (Eq, Ord, Show)

widthFromHints :: Hints -> Width
widthFromHints = Width . length . colHints

heightFromHints :: Hints -> Height
heightFromHints = Height . length . rowHints

-- Store in a compact format
storeHints :: Hints -> String
storeHints hints = store rowHints <> "/" <> store colHints where
	store f = intercalate ";" $ fmap (intercalate "," . fmap show) $ f hints

-- Read the stored hints
parseHints :: String -> Hints
parseHints string = case splitOn '/' string of
	[rowString, colString] -> let
		rowHints = parseAxis rowString
		colHints = parseAxis colString
		parseAxis = fmap parseHint . splitOn ';'
		parseHint "" = []
		parseHint xs = fmap read $ splitOn ',' $ xs
		in Hints {rowHints, colHints}
	_ -> error "Invalid hint string"

hintOne :: [Bool] -> [Int]
hintOne = fmap length . filter head . group

hintAll :: [[Bool]] -> [[Int]]
hintAll = fmap hintOne

hintGrid :: Grid Bool -> Hints
hintGrid grid = Hints { rowHints, colHints } where
	rowHints = hintAll $ getRows grid
	colHints = hintAll $ getCols grid