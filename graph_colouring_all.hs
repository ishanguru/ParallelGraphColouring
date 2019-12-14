{-# LANGUAGE BangPatterns #-}
import qualified Data.Map as Map
import System.Exit(die)
import System.IO(Handle, hIsEOF, hGetLine, withFile, IOMode(ReadMode))
import System.Environment(getArgs, getProgName)
import System.Directory
--import Control.Monad.IO.Class
--import Control.Monad.Par.Combinator
--import Control.Monad.Par.IO as ParIO
import Control.Parallel.Strategies
--import Control.Exception (evaluate)
import Control.DeepSeq

-- resources:
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html#g:5
-- http://ijmcs.future-in-tech.net/11.1/R-Anderson.pdf

type Node = String
type Color = Int
type AdjList = [Node]
type Graph = Map.Map(Node) (AdjList, Color)
-- ghci> import System.IO(stdin)
-- ghci> readGraphLine stdin Map.empty
-- A:B,C,D

-- stack ghc -- --make -Wall -O2 -threaded -rtsopts -eventlog graph_colouring.hs
-- ./graph_colouring samples/CLIQUE_300_3.3color 3
main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  case args of
    [inFolder, number_colours, outFolder] -> do
      filepaths <- filter isValidFile <$> getDirectoryContents inFolder
      let colours = read number_colours
      let solutions = parMap rseq (\f -> colorAGraph f colours outFolder inFolder) filepaths
      
      --mapM_ (\f -> colorAGraph f colours outFolder inFolder) files

      putStrLn "done"

    _ -> do 
        die $ "Usage: " ++ pn ++ " <input-folder> <number-of-colors> <algo: seq or par> <output-folder>"

colorAGraph :: FilePath -> Color -> String -> String -> IO Bool
colorAGraph graph_file colours outFolder inFolder = do
              let outFile = outFolder ++ "/" ++ graph_file ++ "_out"
              g <- readGraphFile $ inFolder ++ graph_file
              let output =  isValidGraph $  colorGraphSeq (Map.keys g) [1..colours] g
              response output graph_file
              writeToFile output outFile
              
              return True
              

isValidFile :: FilePath -> Bool
isValidFile f =  "3color" /=  last ( wordsWhen (=='.') (show f) ) && f /= "." && f /= ".."
              

response :: Maybe Graph -> String -> IO ()
response g fname = case g of
                    Just _ -> do putStrLn $ "Successfully coloured graph " ++ fname
                    Nothing -> do  putStrLn $ "Unable to colour graph "  ++ fname

writeToFile :: Maybe Graph -> String -> IO ()
writeToFile g fout = case g of
                      Just a -> do writeFile fout ("true\n" ++ printSolution a)
                      Nothing -> do writeFile fout "false\n"

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

-- assigns a colour to each node in the graph
-- e.g. colorGraph (Map.keys g) [1,2,3,4] g
colorGraphSeq :: [Node] -> [Color] -> Graph -> Maybe Graph
colorGraphSeq _ [] g = Just g
colorGraphSeq [] _ g = Just g
colorGraphSeq (n:ns) colors g
  | allVerticesColored g = Just g
  | otherwise = 
      if nodeColor > 0 then do
        colorGraphSeq ns colors $ setColor g n nodeColor
      else do
        Nothing
      where nodeColor = colorNode n colors g


colorNode :: Node -> [Color] -> Graph -> Color
colorNode _ [] _ = 0
colorNode n (x:xs) g = if validColor n x g then do x
                              else do colorNode n xs g

-- checks if all vertices have been coloured
-- e.g. allVerticesColored g
allVerticesColored :: Graph -> Bool
allVerticesColored g = 0 `notElem` getColors (Map.keys g) g

isValidGraph :: Maybe Graph -> Maybe Graph
isValidGraph g = case g of
                  Nothing -> Nothing
                  Just a -> isValidGraph' (Map.keys a) a

isValidGraph' :: [Node] -> Graph -> Maybe Graph
isValidGraph' [] _ = Nothing
isValidGraph' [n] g | getColor n g > 0 = Just g
                    | otherwise = Nothing
isValidGraph' (n:ns) g 
    | getColor n g `notElem` getColors (getNeighbors n g) g = isValidGraph' ns g
    | otherwise = Nothing

-- checks if this color can be assigned to a vertex
-- e.g. validColor "A" 1 g
validColor :: Node -> Color -> Graph -> Bool
validColor n c g = c `notElem` getColors (getNeighbors n g) g

printSolution :: Graph -> String
printSolution g = unlines $ map (\n -> n ++ ':' : showColor n ) nodes
                  where nodes = Map.keys g
                        showColor n = show $ getColor n g
