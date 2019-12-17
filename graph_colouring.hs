-- stack ghc -- --make -Wall -O2 -threaded -rtsopts -eventlog graph_colouring.hs
import Utils
import GraphColoringAlgo

import System.Exit(die)
import System.Environment(getArgs, getProgName)

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
        "seq" -> do 
          msg <- colorAGraph graph_file colours outFolder ""
          putStrLn msg
        "par" -> do 
          putStrLn "not implemented"
        "all" -> do
          let inFolder = graph_file
          filepaths <- filter isValidFile <$> getDirectoryContents inFolder
          responses <- runParIO $ parMapM (\f -> liftIO $ colorAGraph f colours outFolder inFolder) filepaths
          mapM_ putStrLn responses
        _ -> do 
          die $ "Usage: " ++ pn ++ " <graph-{file/folder}name/> <number-of-colors> <algo: seq or par> <output-folder>"

    _ -> do 
        die $ "Usage: " ++ pn ++ " <graph-{file/folder}name/> <number-of-colors> <algo: seq or par> <output-folder>"
