{-# LANGUAGE BangPatterns #-}
import Utils
import qualified Data.Map as Map
import System.Exit(die)
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

-- ghci> import System.IO(stdin)
-- ghci> readGraphLine stdin Map.empty
-- A:B,C,D
--

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
      responses <- runParIO $ parMapM (\f -> liftIO $ colorAGraph f colours outFolder inFolder) filepaths
      mapM_ putStrLn responses
      putStrLn "done"

    _ -> do 
        die $ "Usage: " ++ pn ++ " <input-folder> <number-of-colors> <algo: seq or par> <output-folder>"

colorAGraph :: FilePath -> Color -> String -> String -> IO String
colorAGraph graph_file colours outFolder inFolder = do
              let outFile = outFolder ++ "/" ++ graph_file ++ "_out"
              g <- readGraphFile $ inFolder ++ graph_file
              putStrLn ("coloring " ++ graph_file ++ " .. ")
              let output =  isValidGraph $  colorGraph (Map.keys g) [1..colours] [1..colours] g
              response output graph_file
              writeToFile output outFile
              return $ "done" ++ inFolder


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

