-- resources:
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html#g:5
-- http://ijmcs.future-in-tech.net/11.1/R-Anderson.pdf

module Utils
( Node,
  Color,
  AdjList,
  Graph,
  response,
  writeToFile,
  readGraphFile,
  isValidFile,
  getColor,
  getColors,
  getNeighbors,
  setColor,
  validColor,
  allVerticesColored,
  isValidGraph,
  isValidGraphPar
) where

import qualified Data.Map as Map
import System.IO(Handle, hIsEOF, hGetLine, withFile,IOMode(ReadMode))
import Data.Maybe as Maybe
import Control.Parallel.Strategies (rpar, runEval)

type Node = String
type Color = Int
type AdjList = [Node]
type Graph = Map.Map(Node) (AdjList, Color)

response :: Maybe Graph -> String -> IO ()
response g fname = case g of
                    Just _ -> putStrLn ("Successfully coloured graph " ++ fname)
                    Nothing ->  putStrLn ("Unable to colour graph "  ++ fname)

writeToFile :: Maybe Graph -> String -> IO ()
writeToFile g fout = case g of
                      Just a -> do writeFile fout ("true\n" ++ printSolution a)
                      Nothing -> do writeFile fout "false\n"

-- construct a graph from input file
-- g <- readGraphFile "samples/CLIQUE_300_3.3color"
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
validColor :: Node -> Color -> Graph -> Bool
validColor n c g = c `notElem` getColors (getNeighbors n g) g

printSolution :: Graph -> String
printSolution g = unlines $ map (\n -> n ++ ':' : showColor n ) nodes
                  where nodes = Map.keys g
                        showColor n = show $ getColor n g

-- checks if all vertices have been coloured
-- e.g. allVerticesColored g
allVerticesColored :: Graph -> Bool
allVerticesColored g = 0 `notElem` getColors (Map.keys g) g

isValidGraph :: Graph -> Bool
isValidGraph g = isValidGraph' (Map.keys g) g

isValidGraph' :: [Node] -> Graph -> Bool
isValidGraph' [] _ = False
isValidGraph' [n] g = getColor n g >= 0
isValidGraph' (n:ns) g 
    | getColor n g `notElem` getColors (getNeighbors n g) g = isValidGraph' ns g
    | otherwise = False

isValidGraphPar :: Maybe Graph -> Maybe Graph
isValidGraphPar g = case g of
                  Nothing -> Nothing
                  Just a -> isValidGraphPar' (Map.keys a) a

isValidGraphPar' :: [Node] -> Graph -> Maybe Graph
isValidGraphPar' [] _ = Nothing
isValidGraphPar' [n] g | getColor n g `notElem` getColors (getNeighbors n g) g = Just g
                      | otherwise = Nothing
isValidGraphPar' nodes g 
  | runEval $ do
      front <- rpar $ isValidGraphPar' first g
      back <- rpar $ isValidGraphPar' second g
      return (Maybe.isJust front && Maybe.isJust back) = Just g
  | otherwise = Nothing
  where first = take ((length nodes) `div` 2) nodes
        second = drop ((length nodes) `div` 2) nodes
