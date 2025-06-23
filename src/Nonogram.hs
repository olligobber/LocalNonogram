module Nonogram
	( Grid(Grid, getRows)
	, sizeFromGrid
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

import Data.List (transpose, intercalate, group, (!!))

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

sizeFromGrid :: Grid x -> Int
sizeFromGrid = length . getRows

getRow :: Int -> Grid x -> [x]
getRow n = (!! n) . getRows

getCols :: Grid x -> [[x]]
getCols = transpose . getRows

getCol :: Int -> Grid x -> [x]
getCol n = fmap (!! n) . getRows

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
parseHints string = Hints {rowHints, colHints} where
	[rowString, colString] = splitOn '/' string
	rowHints = parseAxis rowString
	colHints = parseAxis colString
	parseAxis = fmap parseHint . splitOn ';'
	parseHint "" = []
	parseHint xs = fmap read $ splitOn ',' $ xs

hintOne :: [Bool] -> [Int]
hintOne = fmap length . filter head . group

hintAll :: [[Bool]] -> [[Int]]
hintAll = fmap hintOne

hintGrid :: Grid Bool -> Hints
hintGrid grid = Hints { rowHints, colHints } where
	rowHints = hintAll $ getRows grid
	colHints = hintAll $ getCols grid