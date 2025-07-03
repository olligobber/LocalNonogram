-- Count how many locally solvable nonograms there are of a given size
-- Input is the size, output is the count

import Control.Monad (replicateM)
import Data.Proxy (Proxy(..))
import Nonogram (Grid(Grid), Hints, hintGrid)
import SolveLocally (Solution(Solved), solveGrid)
import ArrayGrid (ArrayGrid)

allBool :: [Bool]
allBool = [False, True]

allRow :: Int -> [[Bool]]
allRow size = replicateM size allBool

allGrid :: Int -> [Grid Bool]
allGrid size = Grid <$> replicateM size (allRow size)

allHints :: Int -> [Hints]
allHints size = hintGrid <$> allGrid size

isSolved :: Solution -> Bool
isSolved (Solved _) = True
isSolved _ = False

solvableHints :: Int -> [Hints]
solvableHints size =
	filter (isSolved . solveGrid (Proxy @ArrayGrid)) $
	allHints size

main :: IO ()
main = readLn >>= print . length . solvableHints