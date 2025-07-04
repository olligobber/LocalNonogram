-- Count how many locally solvable nonograms there are of a given size
-- Input is the size,
	-- if one value is provided it is a square with that sidelength,
	-- if two it is a rectamgle with those dimensions
-- Output is the count

import Control.Monad (replicateM)
import Data.Proxy (Proxy(..))
import Nonogram (Grid(Grid), Width(..), Height(..), Hints, hintGrid)
import SolveLocally (Solution(Solved), solveGrid)
import ArrayGrid (ArrayGrid)

type WithSize x = (Width, Height) -> x

width :: WithSize Width
width = fst

height :: WithSize Height
height = snd

allBool :: [Bool]
allBool = [False, True]

allRow :: WithSize [[Bool]]
allRow = do
	w <- width
	pure $ replicateM (fromWidth w) allBool

allGrid :: WithSize [Grid Bool]
allGrid = do
	h <- height
	allRows <- allRow
	pure $ Grid <$> replicateM (fromHeight h) allRows

allHints :: WithSize [Hints]
allHints = fmap hintGrid <$> allGrid

isSolved :: Solution -> Bool
isSolved (Solved _) = True
isSolved _ = False

solvableHints :: WithSize [Hints]
solvableHints =
	filter (isSolved . solveGrid (Proxy @ArrayGrid)) <$> allHints

main :: IO ()
main = do
	inputVals <- fmap read . words <$> getLine
	let (w, h) = case inputVals of
		[n] -> (Width n, Height n)
		[a, b] -> (Width a, Height b)
		_ -> error "Wrong number of input values, please provide one for a square or two for a rectangle"
	print $ length $ solvableHints (w, h)