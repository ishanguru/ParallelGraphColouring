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
  validColor
) where

import qualified Data.Map as Map
import System.IO(Handle, hIsEOF, hGetLine, withFile,IOMode(ReadMode))

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
