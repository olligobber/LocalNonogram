module Nonogram
	( Grid(Grid, getRows)
	, sizeFromGrid
	, Vertical(Vertical, fromVertical)
	, Horizontal(Horizontal, fromHorizontal)
	, getRow
	, getCols
	, getCol
	, storeGrid
	, parseGrid
	, Hints(Hints, colHints, rowHints)
	, sizeFromHints
	, storeHints
	, parseHints
	, hintOne
	, hintGrid
	)
where

import Data.List (transpose, intercalate, group)
import Data.Ix (Ix)

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

sizeFromGrid :: Grid x -> Int
sizeFromGrid = length . getRows

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

sizeFromHints :: Hints -> Int
sizeFromHints = length . rowHints

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