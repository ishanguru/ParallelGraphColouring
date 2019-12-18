-- stack ghc -- --make -Wall -O2 -threaded -rtsopts -eventlog graph_colouring.hs
import Utils
import GraphColoringAlgo

import System.Exit(die)
import System.Environment(getArgs, getProgName)

import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Combinator (parMapM)
import Control.Monad.Par.IO (runParIO)
import System.Directory

main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  case args of
    [graph_file, number_colours, algo, method, outFolder] -> do
      func <- case algo of
        "backtracking" -> do
                let colours = read number_colours
                return (\g -> backtracking (Map.keys g) [1..colours] [1..colours] g)
        "IndepSet" -> return (\g -> colorIndependent g g (Map.keys g) [1..])
        "greedy" -> return (\g -> backtracking (Map.keys g) [1..] [1..] g)

      case method of
        "file" -> do
          msg <- colorAGraph graph_file func outFolder ""
          putStrLn msg
        "folder" -> do
          let inFolder = graph_file
          filepaths <- filter isValidFile <$> getDirectoryContents inFolder
          responses <- runParIO $ parMapM (\f -> liftIO $ colorAGraph f func outFolder inFolder) filepaths
          mapM_ putStrLn responses
        _ -> do 
          die $ "Usage: " ++ pn ++ " <graph-{file/folder}name/> <number-of-colors> <algo: seq or par> <output-folder>"
        
    _ -> do 
        die $ "Usage: " ++ pn ++ " <graph-{file/folder}name/> <number-of-colors> <algo: seq or par> <output-folder>"
