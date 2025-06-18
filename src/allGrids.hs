-- Take in the size and output all possible grids

import Control.Monad (replicateM)
import Nonogram (Grid(Grid), storeGrid)

allBool :: [Bool]
allBool = [False, True]

allRow :: Int -> [[Bool]]
allRow size = replicateM size allBool

allGrid :: Int -> [Grid Bool]
allGrid size = Grid <$> replicateM size (allRow size)

main :: IO ()
main = do
	size <- readLn
	mapM_ (putStrLn . storeGrid) $ allGrid size