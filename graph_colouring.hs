-- module GraphColoring (readGraphFile) where
import qualified Data.Map as Map
import System.Exit(die)
import System.IO(Handle, hIsEOF, hGetLine, withFile, IOMode(ReadMode))
import System.Environment(getArgs, getProgName)

-- resources:
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html#g:5
-- http://ijmcs.future-in-tech.net/11.1/R-Anderson.pdf
-- 
-- For sequential implementation, should we use this algorithm?
--  https://www.geeksforgeeks.org/m-coloring-problem-backtracking-5/ll

-- figure out how to represent a graph
type Node = String
type Color = Int
type AdjList = [Node]
type Graph = Map.Map(Node) (AdjList, Color)
-- ghci> import System.IO(stdin)
-- ghci> readGraphLine stdin Map.empty
-- A:B,C,D

-- stack ghc -- --make -Wall -O graph_colouring.hs
-- ./graph_colouring samples/CLIQUE_300_3.3color 3
main :: IO ()
main = do
  args <- getArgs
  case args of
    [graph_file, number_colours] -> do
      let colours = read number_colours
      g <- readGraphFile graph_file
      let result = isValidGraph $ colorGraph (Map.keys g) [1..colours] g
      if result then do 
        putStrLn "Successfully coloured graph"
      else do 
        putStrLn "Unable to colour graph"

    _ -> do 
        pn <- getProgName
        die $ "Usage: " ++ pn ++ " <graph-filename> <number-of-colors>"


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

-- given a list of nodes and a graph, retrieve all colour assignments to the node
color' :: [Node] -> Graph -> [Color]
color' [] _ = []
color' (x:xs) g = color x g : color' xs g

-- gets all neighbors for a node in a graph
getNeighbors :: Node -> Graph -> AdjList
getNeighbors n g = case Map.lookup n g of
                 Just v -> (fst v)
                 Nothing -> []

-- assings a colour to a single node in the graph
setColor :: Graph -> Node -> Color -> Graph
setColor g n c = case Map.lookup n g of
                  Just v -> Map.insert n ((fst v), c) g
                  Nothing -> g

-- assigns a colour to each node in the graph
-- e.g. colorGraph (Map.keys g) [1,2,3,4] g
colorGraph :: [Node] -> [Color] -> Graph -> Graph
colorGraph _ [] g = g
colorGraph [] _ g = g
colorGraph nodes@(n:ns) colors@(x:xs) g
  | allVerticesColored g = g
  | otherwise = 
      if nodeColor >0  then do
        colorGraph ns colors $ setColor g n nodeColor
      else do
        error "can't color graph"
      where nodeColor = colorNode n colors g

colorNode :: Node -> [Color] -> Graph -> Color
colorNode _ [] _ = 0
colorNode n colors@(x:xs) g = if validColor n x g then do x
                              else do colorNode n xs g

-- checks if all vertices have been coloured
-- e.g. allVerticesColored g
allVerticesColored :: Graph -> Bool
allVerticesColored g = 0 `notElem` color' (Map.keys g) g

isValidGraph :: Graph -> Bool
isValidGraph g = isValidGraph' (Map.keys g) g

isValidGraph' :: [Node] -> Graph -> Bool
isValidGraph' [] g = False
isValidGraph' [n] g = color n g >= 0
isValidGraph' (n:ns) g 
    | color n g `notElem` color' (getNeighbors n g) g = isValidGraph' ns g
    | otherwise = False

-- checks if this color can be assigned to a vertex
-- e.g. validColor "A" 1 g
validColor :: Node -> Color -> Graph -> Bool
validColor n c g = c `notElem` color' (getNeighbors n g) g


