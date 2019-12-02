-- resources:
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html#g:5
-- http://ijmcs.future-in-tech.net/11.1/R-Anderson.pdf
-- 
-- For sequential implementation, should we use this algorithm?
--  https://www.geeksforgeeks.org/m-coloring-problem-backtracking-5/

import qualified Data.Map as Map
import System.IO(Handle, hIsEOF, hGetLine, withFile, IOMode(ReadMode))
-- figure out how to represent a graph
type Node = Int
type Color = Int
type Edge = (Node, Node)
type AdjList = [Node]
type Graph = Map.Map(Node) (AdjList, Color)
-- construct a graph from input file
-- TODO: decide the format of input file
-- g <- readGraphFile "sample.txt"
readGraphLine :: Handle -> Graph -> IO Graph
readGraphLine handle g = do [node, color, adj] <- words <$> hGetLine handle
                            return $ Map.insert (read node) (readAdjList adj, (read color)) g

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
readAdjList x = map (\w -> read w) $ wordsWhen (==',') x
-- implements fromList/toList?
-- color the graph and output a valid color to the vertices or false if it doesnt exist

-- degree of a vertex
degree :: Node -> Graph -> Int
degree n g = case Map.lookup n g of
                  Just v -> length $ fst v
                  Nothing -> 0

-- clor of a vertex
color :: Node -> Graph -> Color
color n g = case Map.lookup n g of
                 Just v -> (snd v)
                 Nothing -> error "not valid graph"
