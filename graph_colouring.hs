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
    [graph_file, number_colours, algo, outFolder] -> do
      let colours = read number_colours
      case algo of
        "divide-conquer" -> do 
          g <- readGraphFile graph_file
          let max_colours = length $ Map.keys g
          let output = divideConquerPar (Map.keys g) [1..max_colours] g
          let result = isValidGraph $ output
          case result of
            True -> putStrLn "Successfully coloured graph"
            _ -> putStrLn "Unable to colour graph"
        "backtracking" -> do
          let inFolder = graph_file
          filepaths <- filter isValidFile <$> getDirectoryContents inFolder
          responses <- runParIO $ parMapM (\f -> liftIO $ colorAGraph f colours outFolder inFolder) filepaths
          mapM_ putStrLn responses
        "IndepSet" -> do
          -- following algo in:
          -- http://www.ii.uib.no/~assefaw/pub/coloring/thesis.pdf
          -- an order of mag slower than backtracking algorithm
          g <- readGraphFile graph_file
          let graph_file_name = last $ wordsWhen (=='/') graph_file
          let outFile = outFolder ++ "/" ++ graph_file_name ++ "_out"
          let output = checkValidColored $ colorIndependent g g (Map.keys g) [1..colours]
          response output graph_file
          writeToFile output outFile
        _ -> do 
          die $ "Usage: " ++ pn ++ " <graph-{file/folder}name/> <number-of-colors> <algo: {divide-conquer/backtracking/IndepSet}> <output-folder>"

    _ -> do 
        die $ "Usage: " ++ pn ++ " <graph-{file/folder}name/> <number-of-colors> <algo: {divide-conquer/backtracking/IndepSet}> <output-folder>"
