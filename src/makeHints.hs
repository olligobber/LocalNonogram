-- Given a grid on each line, output its hints on each line

import Nonogram (parseGrid, storeHints, hintGrid)

main :: IO ()
main = interact $ unlines . fmap (storeHints . hintGrid . parseGrid) . lines