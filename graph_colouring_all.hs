{-# LANGUAGE BangPatterns #-}
import qualified Data.Map as Map
import System.Exit(die)
import System.IO(Handle, hIsEOF, hGetLine, withFile,IOMode(ReadMode))
import System.Environment(getArgs, getProgName)
import System.Directory
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Combinator (parMapM)
import Control.Monad.Par.IO (runParIO)
import Control.Parallel.Strategies
import Data.Maybe as Maybe
-- resources:
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- https://hackage.haskell.org/package/containers-0.4.2.0/docs/Data-Map.html#g:5
-- http://ijmcs.future-in-tech.net/11.1/R-Anderson.pdf
-- https://gist.github.com/nh2/bfa3f182da9d13baa536

type Node = String
type Color = Int
type AdjList = [Node]
type Graph = Map.Map(Node) (AdjList, Color)
-- ghci> import System.IO(stdin)
-- ghci> readGraphLine stdin Map.empty
-- A:B,C,D
--

foldrM' :: Monad m => (a -> m b -> m b) -> m b -> [m a] -> m b
foldrM' f z = act
  where
    act [] = z
    act (x:xs) = ($ act xs) . f =<< x
    -- Or: act (x:xs) = x >>= \x' -> f x' $ act xs


-- stack ghc -- --make -Wall -O2 -threaded -rtsopts -eventlog graph_colouring.hs
-- ./graph_colouring samples/CLIQUE_300_3.3color 3
main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  case args of
    [inFolder, number_colours, outFolder] -> do
      filepaths <- filter isValidFile <$> getDirectoryContents inFolder
      print filepaths
      let colours = read number_colours
      responses <- runParIO $ parMapM (\f -> liftIO $ colorAGraph f colours outFolder inFolder) filepaths
      mapM_ putStrLn responses

      --start <- getCPUTime
      --let problems = colorAll filepaths colours inFolder outFolder
      --end <- r `deepseq` getCPUTime
      
      --let solutions = parMap rseq (\f -> colorAGraph f colours outFolder inFolder) filepaths
      -- let graphs = parMap rseq (\f -> readGraphFile $ inFolder ++ f) filepaths
      -- let solutions = map (\f -> colorGraph f "test" colours outFolder inFolder) graphs `using` parList rseq
      -- let solutions = colorAllGraph filepaths colours outFolder inFolder

      --print =<< foldrM' (\acc x -> x >>= \x' -> return (acc++ " "++ x')) (return "") solutions
      --mapM_ (\f -> colorAGraph f colours outFolder inFolder) files

      putStrLn "done"

    _ -> do 
        die $ "Usage: " ++ pn ++ " <input-folder> <number-of-colors> <algo: seq or par> <output-folder>"

colorAGraph :: FilePath -> Color -> String -> String -> IO String
colorAGraph graph_file colours outFolder inFolder = do
              let outFile = outFolder ++ "/" ++ graph_file ++ "_out"
              g <- readGraphFile $ inFolder ++ graph_file
              let output =  isValidGraph $  colorGraph (Map.keys g) [1..colours] [1..colours] g
              response output graph_file
              writeToFile output outFile
              return $ "done" ++ inFolder

{-

colorAllGraph :: [FilePath] -> Color -> String -> String -> [IO String]
colorAllGraph [x] colours outFolder inFolder = [colorAGraph x colours outFolder inFolder]
colorAllGraph graph_files colours outFolder inFolder = runEval $ do
    front <- rpar $ colorAllGraph first colours outFolder inFolder
    back <- rpar $ colorAllGraph second colours outFolder inFolder
    return $ front ++ back
  where first = take ((length graph_files) `div` 2) graph_files
        second = drop ((length graph_files) `div` 2) graph_files
-}              

isValidFile :: FilePath -> Bool
isValidFile f =  "3color" /=  last ( wordsWhen (=='.') (show f) ) && f /= "." && f /= ".."
              

response :: Maybe Graph -> String -> IO ()
response g fname = case g of
                    Just _ -> putStrLn ("Successfully coloured graph " ++ fname)
                    Nothing ->  putStrLn ("Unable to colour graph "  ++ fname)

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
colorGraph :: [Node] -> [Color] -> [Color] -> Graph -> Maybe Graph
colorGraph _ [] _ g = Just g
colorGraph [] _ _ g = Just g
colorGraph _ _ [] _ = Nothing
colorGraph nodes@(n:ns) colors (c:cs) g
      | validColor n c g = case (colorGraph ns colors colors $ setColor g n c) of
                              Just gout -> Just gout
                              Nothing -> colorGraph nodes colors cs g
      | otherwise = colorGraph nodes colors cs g

isValidGraph :: Maybe Graph -> Maybe Graph
isValidGraph g = case g of
                  Nothing -> Nothing
                  Just a -> isValidGraphPar (Map.keys a) a

isValidGraphPar :: [Node] -> Graph -> Maybe Graph
isValidGraphPar [] _ = Nothing
isValidGraphPar [n] g | getColor n g `notElem` getColors (getNeighbors n g) g = Just g
                      | otherwise = Nothing
isValidGraphPar nodes g 
  | runEval $ do
      front <- rpar $ isValidGraphPar first g
      back <- rpar $ isValidGraphPar second g
      return (Maybe.isJust front && Maybe.isJust back) = Just g
  | otherwise = Nothing
  where first = take ((length nodes) `div` 2) nodes
        second = drop ((length nodes) `div` 2) nodes

-- checks if this color can be assigned to a vertex
-- e.g. validColor "A" 1 g
validColor :: Node -> Color -> Graph -> Bool
validColor n c g = c `notElem` getColors (getNeighbors n g) g

printSolution :: Graph -> String
printSolution g = unlines $ map (\n -> n ++ ':' : showColor n ) nodes
                  where nodes = Map.keys g
                        showColor n = show $ getColor n g
