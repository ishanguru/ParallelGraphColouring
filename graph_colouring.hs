-- stack ghc -- --make -Wall -O2 -threaded -rtsopts -eventlog graph_colouring.hs
import Utils
import qualified Data.Map as Map
import System.Exit(die)
import System.Environment(getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  case args of
    [graph_file, number_colours, algo, outFolder] -> do
      let colours = read number_colours
      let outFile = outFolder ++ "/" ++ graph_file ++ "_out"
      g <- readGraphFile graph_file
      case algo of
        "seq" -> do 
          let output = colorGraph (Map.keys g) [1..colours] [1..colours] g
          response output graph_file
          writeToFile output outFile
        "par" -> do 
          let output = colorGraphPar (Map.keys g) [1..colours] g
          response output graph_file
          writeToFile output outFile
        _ -> do 
          die $ "Usage: " ++ pn ++ " <graph-filename> <number-of-colors> <algo: seq or par> <output-folder>"

    _ -> do 
        die $ "Usage: " ++ pn ++ " <graph-filename> <number-of-colors> <algo: seq or par> <output-folder>"

-- assigns a colour to each node in the graph
-- e.g. colorGraph (Map.keys g) [1,2,3,4] g
colorGraph :: [Node] -> [Color] -> [Color] -> Graph -> Maybe Graph
colorGraph _ _ [] _ = Nothing
colorGraph _ [] _ g = Just g
colorGraph [] _ _ g = Just g
colorGraph nodes@(n:ns) colors (c:cs) g
      | validColor n c g = case (colorGraph ns colors colors $ setColor g n c) of
                              Just gout -> Just gout
                              Nothing -> colorGraph nodes colors cs g
      | otherwise = colorGraph nodes colors cs g

colorGraphPar :: [Node] -> [Color] -> Graph -> Maybe Graph
colorGraphPar _ [] g = Just g
colorGraphPar [] _ g = Just g
colorGraphPar (n:ns) colors g
  | allVerticesColored g = Just g
  | otherwise = 
      if nodeColor > 0 then do
        colorGraphPar ns colors $ setColor g n nodeColor
      else do
        Nothing
      where nodeColor = colorNode n colors g

colorNode :: Node -> [Color] -> Graph -> Color
colorNode _ [] _ = 0
colorNode n (x:xs) g = if validColor n x g then do x
                              else do colorNode n xs g

