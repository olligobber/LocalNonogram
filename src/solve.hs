-- Given a hint on each line, output
	-- 0 if it is contradictory (no solution)
	-- 1 if it is logically solvable (one solution)
	-- 2 if it is not logically solvable (no idea how many solutions)

import Data.List (elemIndex, (!!))
import Nonogram (
	Grid(Grid), getRow, getCol,
	Hints(rowHints, colHints), sizeFromHints, parseHints
	)

-- TODO refactor to use monads, this sucks

data CellInfo = Full | Empty | Unknown deriving (Eq)

-- Merge data about two cells
mergeCell :: CellInfo -> CellInfo -> Maybe CellInfo
mergeCell a Unknown = Just a
mergeCell Unknown a = Just a
mergeCell a b
	| a == b = Just a
	| otherwise = Nothing

-- Merge data about two lines
mergeLine :: [CellInfo] -> [CellInfo] -> Maybe [CellInfo]
mergeLine [] [] = Just []
mergeLine (a:as) (b:bs) = (:) <$> mergeCell a b <*> mergeLine as bs

type PartGrid = Grid CellInfo

-- Merge data about one row into a grid
mergeRow :: [CellInfo] -> PartGrid -> Maybe PartGrid


-- Blank grid with nothing determined
newGrid :: Int -> PartGrid
newGrid size = Grid $ replicate size $ replicate size $ Unknown

-- Given hints for a line and the current state of the line,
-- output an inferred new line, or Nothing if there is a contradiction
infer :: [Int] -> [CellInfo] -> Maybe [CellInfo]
infer hints line =
	case
		(,) <$>
		placeEarliest hints line <*>
		placeEarliest (reverse hints) (reverse line)
	of
		Nothing -> Nothing
		Just (earliestStarts, x) ->
			let
				size = length line
				latestEnds = (\y -> size - 1 - y) <$> reverse x
				hintData = zip3 hints earliestStarts latestEnds
			in
				Just $ fillBetween hintData line

-- Given hints for a line and the current state of a line,
-- output the earliest start position for each hint segment,
-- or Nothing if there is a contradiction
placeEarliest :: [Int] -> [CellInfo] -> Maybe [Int]
placeEarliest [] _ = Just []
placeEarliest (h:_) line | h > length line = Nothing
placeEarliest [h] line
	| h == length line && all (/= Empty) line = Just [0]
	| h == length line = Nothing
placeEarliest (h:_) line | h > length line + 1 = Nothing
placeEarliest hs@(h:_) line | line !! h == Full =
	fmap (fmap (+1)) $ placeEarliest hs $ tail line
placeEarliest (h:hs) line = case elemIndex Empty (take h line) of
	Just k -> placeEarliest (h:hs) $ drop k line
	Nothing -> fmap ((0:) . fmap (+(h+1))) $ placeEarliest hs $ drop (h+1) line


-- Given a list of hints, each with a length, earliest start, and latest end
-- and a line, output a new inferred line
fillBetween :: [(Int, Int, Int)] -> [CellInfo] -> [CellInfo]
fillBetween [] line = line
fillBetween hints@((_, start, _):_) line | start > 0 =
	take start line <>
	fillBetween (nudgeHint (-start) <$> hints) (drop start hint)
fillBetween ((size, 0, end):hints) line | end == size =
	replicate end Full <>
	[Empty] <>
	fillBetween (nudgeHint (-(end+1)) <$> hints) (drop (end+1) hint)
fillBetween ((size, 0, end):hints) line | end >= 2 * size =
	fillBetween hints line
fillBetween ((size, 0, end):hints) line =


-- Offset a hint by an amount
nudgeHint :: Int -> (Int, Int, Int) -> (Int, Int, Int)
nudgeHint d (size, start, end) = (size, start + d, end + d)