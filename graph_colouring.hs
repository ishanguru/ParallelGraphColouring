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
import System.Random.Shuffle

main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  let errorMsg = "Usage: " ++ pn ++ " <input-{filename/foldername}> <number-of-colors> <algo: {divide-conquer/backtracking/IndepSet/greedy}> <method: file/folder> <output-folder>"
  case args of
    [graph_file, number_colours, algo, method, outFolder] -> do
      -- select the algorithm to run
      func <- case algo of
        "backtracking" -> do
                let colours = read number_colours
                return (\g -> backtracking (Map.keys g) [1..colours] [1..colours] g)
        "indep-set" -> return (\g -> colorIndependent g g (Map.keys g) [1..])
        "greedy" -> return (\g -> backtracking (Map.keys g) [1..] [1..] g)
        "divide-conquer" -> do 
          return (\g -> divideConquerPar (Map.keys g) g)
        _ -> do die errorMsg

      -- run a file or many files in a folder
      case method of
        "file" -> do
          msg <- colorAGraph graph_file func outFolder ""
          putStrLn msg
        "folder" -> do
          let inFolder = graph_file
          filepaths <- filter isValidFile <$> getDirectoryContents inFolder
          filepathShuffled <- shuffleM filepaths
          putStrLn $ "coloring: \n" ++  (unlines filepathShuffled)
          responses <- runParIO $ parMapM (\f -> liftIO $ colorAGraph f func outFolder inFolder) filepathShuffled
          mapM_ putStrLn responses
        _ -> do 
          die errorMsg
        
    _ -> do 
          die errorMsg
