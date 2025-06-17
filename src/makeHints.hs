-- Given a grid on each line, output its hints on each line

import Data.List (group)
import Nonogram (Grid(..), getCols, parseGrid, Hints(..))

hintOne :: [Bool] -> [Int]
hintOne = fmap length . filter head . group

hintAll :: [[Bool]] -> [[Int]]
hintAll = fmap hintOne

hintGrid :: Grid Bool -> Hints
hintGrid grid = Hints { rowHints, colHints } where
	rowHints = hintAll $ getRows grid
	colHints = hintAll $ getCols grid

main :: IO ()
main = interact $ unlines . fmap (show . hintGrid . parseGrid) . lines