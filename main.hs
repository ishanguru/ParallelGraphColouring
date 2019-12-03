module GraphColoring (readGraphFile) where
import qualified Data.Map as Map
import System.IO(Handle, hIsEOF, hGetLine, withFile, IOMode(ReadMode))

-- resources:
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html#g:5
-- http://ijmcs.future-in-tech.net/11.1/R-Anderson.pdf
-- 
-- For sequential implementation, should we use this algorithm?
--  https://www.geeksforgeeks.org/m-coloring-problem-backtracking-5/

-- figure out how to represent a graph
type Node = String
type Color = Int
type Edge = (Node, Node)
type AdjList = [Node]
type Graph = Map.Map(Node) (AdjList, Color)
-- ghci> import System.IO(stdin)
-- ghci> readGraphLine stdin Map.empty
-- A:B,C,D
readGraphLine :: Handle -> Graph -> IO Graph
readGraphLine handle g = do args  <- (wordsWhen (==':')) <$> hGetLine handle
                            case args of
                              [node, adj] -> return $ Map.insert node (readAdjList adj, 0) g
                              _           -> return g
                            

-- construct a graph from input file
-- g <- readGraphFile "samples/CLIQUE_300_3.3color"
readGraphFile :: String -> IO Graph
readGraphFile filename  = withFile filename ReadMode $ \handle -> loop handle readGraphLine Map.empty

loop :: Handle -> (Handle -> Graph -> IO Graph) -> Graph -> IO Graph
loop h f g = do 
              outgraph <- f h g
              eof <- hIsEOF h
              if eof then do return outgraph
              else do (loop h f outgraph) >>= (\y -> return y)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                         where (w, s'') = break p s'

readAdjList :: String ->  AdjList
readAdjList x = wordsWhen (==',') x

-- degree of a vertex
-- e.g. degree "A" g
degree :: Node -> Graph -> Int
degree n g = case Map.lookup n g of
                  Just v -> length $ fst v
                  Nothing -> 0

-- color of a vertex
-- e.g. color "A" g
color :: Node -> Graph -> Color
color n g = case Map.lookup n g of
                 Just v -> (snd v)
                 Nothing -> error "not valid graph"
