{-# LANGUAGE BangPatterns #-}
import Utils
import qualified Data.Map as Map
import System.Exit(die)
import System.Environment(getArgs, getProgName)
import System.Directory
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Combinator (parMapM)
import Control.Monad.Par.IO (runParIO)

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
              let output =  isValidGraphPar $  colorGraph (Map.keys g) [1..colours] [1..colours] g
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


