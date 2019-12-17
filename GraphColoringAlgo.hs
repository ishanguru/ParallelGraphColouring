module GraphColoringAlgo
( colorGraph,
  colorAGraph
) where

import Utils

import qualified Data.Map as Map

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

colorAGraph :: FilePath -> Color -> String -> String -> IO String
colorAGraph graph_file colours outFolder inFolder = do
              let outFile = outFolder ++ "/" ++ graph_file ++ "_out"
              g <- readGraphFile $ inFolder ++ graph_file
              putStrLn ("coloring " ++ graph_file ++ " .. ")
              let output =  checkValidColored $  colorGraph (Map.keys g) [1..colours] [1..colours] g
              response output graph_file
              writeToFile output outFile
              return $ "done coloring " ++ inFolder


