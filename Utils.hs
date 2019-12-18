-- resources:
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html#g:5
-- http://ijmcs.future-in-tech.net/11.1/R-Anderson.pdf
-- Parallel IO -> https://gist.github.com/nh2/bfa3f182da9d13baa536

module Utils
( Node,
  Color,
  AdjList,
  Graph,
  response,
  wordsWhen,
  writeToFile,
  readGraphFile,
  isValidFile,
  getColor,
  getColors,
  getNeighbors,
  setColor,
  validColor,
  allVerticesColored,
  checkValidColored,
  checkValidColoredPar,
  findClashingNodes,
  colorNode,
  setColors,
  isValidGraph
) where

import qualified Data.Map as Map
import System.IO(Handle, hIsEOF, hGetLine, withFile,IOMode(ReadMode))
import Data.Maybe as Maybe
import Control.Parallel.Strategies (rpar, runEval)

-- Type to define a Graph as adjecncy list
type Node = String
type Color = Int
type AdjList = [Node]
type Graph = Map.Map(Node) (AdjList, Color)

-- check if <graph> from <fname> is {un}successfully colored and prints out a message
response :: Maybe Graph -> String -> IO ()
response graph fname = case graph of
                    Just _ -> putStrLn ("Successfully coloured graph " ++ fname)
                    Nothing ->  putStrLn ("Unable to colour graph "  ++ fname)

-- write <graph> to <fout>
writeToFile :: Maybe Graph -> String -> IO ()
writeToFile graph fout = case graph of
                      Just a -> do writeFile fout ("true\n" ++ printSolution a)
                      Nothing -> do writeFile fout "false\n"

-- construct a graph from <filename>
-- ghci> g <- readGraphFile "samples/CLIQUE_300_3.3color"
readGraphFile :: String -> IO Graph
readGraphFile filename  = withFile filename ReadMode $ \handle -> loop handle readGraphLine Map.empty

-- reads a single line as graph
-- ghci> import System.IO(stdin)
-- ghci> readGraphLine stdin Map.empty
-- A:B,C,D
readGraphLine :: Handle -> Graph -> IO Graph
readGraphLine handle g = do args  <- (wordsWhen (==':')) <$> hGetLine handle
                            case args of
                              [node, adj] -> return $ Map.insert node (readAdjList adj, 0) g
                              _           -> return g

isValidFile :: FilePath -> Bool
isValidFile f =  "3color" /=  last ( wordsWhen (=='.') (show f) ) && f /= "." && f /= ".."

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
-- color of a vertex
-- e.g. color "A" g
getColor :: Node -> Graph -> Color
getColor n g = case Map.lookup n g of
                 Just v -> (snd v)
                 Nothing -> 0

-- given a list of nodes and a graph, retrieve all colour assignments to the node
getColors :: [Node] -> Graph -> [Color]
getColors [] _ = []
getColors (x:xs) g = getColor x g : getColors xs g

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


readAdjList :: String ->  AdjList
readAdjList x = wordsWhen (==',') x

-- checks if this color can be assigned to a vertex
-- e.g. validColor "A" 1 g
validColor :: Node -> Graph -> Color -> Bool
validColor n g c = c `notElem` getColors (getNeighbors n g) g

printSolution :: Graph -> String
printSolution g = unlines $ map (\n -> n ++ ':' : showColor n ) nodes
                  where nodes = Map.keys g
                        showColor n = show $ getColor n g

-- checks if all vertices have been coloured
-- e.g. allVerticesColored g
allVerticesColored :: Graph -> Bool
allVerticesColored g = 0 `notElem` getColors (Map.keys g) g

checkValidColoredPar :: Maybe Graph -> Maybe Graph
checkValidColoredPar g = case g of
                  Nothing -> Nothing
                  Just a -> checkValidColoredPar' (Map.keys a) a

checkValidColoredPar' :: [Node] -> Graph -> Maybe Graph
checkValidColoredPar' [] _ = Nothing
checkValidColoredPar' [n] g | getColor n g `notElem` getColors (getNeighbors n g) g = Just g
                      | otherwise = Nothing
checkValidColoredPar' nodes g 
  | runEval $ do
      front <- rpar $ checkValidColoredPar' first g
      back <- rpar $ checkValidColoredPar' second g
      return (Maybe.isJust front && Maybe.isJust back) = Just g
  | otherwise = Nothing
  where first = take ((length nodes) `div` 2) nodes
        second = drop ((length nodes) `div` 2) nodes

checkValidColored :: Maybe Graph -> Maybe Graph
checkValidColored g = case g of
                  Nothing -> Nothing
                  Just a -> checkValidColored' (Map.keys a) a

checkValidColored' :: [Node] -> Graph -> Maybe Graph
checkValidColored' [] g = Just g
checkValidColored' (n:ns) g 
    | getColor n g `notElem` getColors (getNeighbors n g) g = checkValidColored' ns g
    | otherwise = Nothing


findClashingNodes :: Node -> Graph -> [Node]
findClashingNodes n g = [ x | x <- (getNeighbors n g), (getColor n g) == (getColor x g) ]


colorNode :: Node -> [Color] -> Graph -> Color
colorNode _ [] _ = 0
colorNode n (x:xs) g = if validColor n g x then do x
                              else do colorNode n xs g

setColors :: Graph -> [Node] -> Color -> Graph
setColors g [] _ = g
setColors g [n] c = setColor g n c
setColors g (n:ns) c = setColors (setColor g n c) ns c   

isValidGraph :: Graph -> Bool
isValidGraph g = isValidGraphPar (Map.keys g) g

isValidGraphPar :: [Node] -> Graph -> Bool
isValidGraphPar [] _ = False
isValidGraphPar [n] g = getColor n g `notElem` getColors (getNeighbors n g) g
isValidGraphPar nodes g = runEval $ do
    front <- rpar $ isValidGraphPar first g
    back <- rpar $ isValidGraphPar second g
    return $ front && back
    where (first, second) = splitAt (length nodes `div` 2) nodes                           
