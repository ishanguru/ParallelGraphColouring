module GraphColoringAlgo
( backtracking,
  colorAGraph,
  colorIndependent
) where

import Utils

import qualified Data.Map as Map
import Control.Parallel.Strategies (rpar, rseq, runEval, parListChunk, using, parMap, parBuffer)

-- assigns a colour to each node in the graph
-- e.g. backtracking (Map.keys g) [1,2,3,4] g
backtracking :: [Node] -> [Color] -> [Color] -> Graph -> Maybe Graph
backtracking _ [] _ g = Just g
backtracking [] _ _ g = Just g
backtracking _ _ [] _ = Nothing
backtracking nodes@(n:ns) colors (c:cs) g
      | validColor n c g = case (backtracking ns colors colors $ setColor g n c) of
                              Just gout -> Just gout
                              Nothing -> backtracking nodes colors cs g
      | otherwise = backtracking nodes colors cs g

colorAGraph :: FilePath -> Color -> String -> String -> IO String
colorAGraph graph_file colours outFolder inFolder = do
              let graph_file_name = last $ wordsWhen (=='/') graph_file
              let outFile = outFolder ++ "/" ++ graph_file_name ++ "_out"
              g <- readGraphFile $ inFolder ++ graph_file
              putStrLn ("coloring " ++ graph_file ++ " .. ")
              let output =  checkValidColored $  backtracking (Map.keys g) [1..colours] [1..colours] g
              response output graph_file
              writeToFile output outFile
              return $ "done coloring " ++ graph_file

-- g = fromList [("A",(["B","C"],0)),("B",(["A","C","D","E","F"],0)),
-- ("C",(["A","B","D"],0)),("D",(["B","C","E"],0)),("E",(["B","D","F"],0)),("F",(["B","E"],0))]
-- U = ["A","B","C","D","E","F"]
-- v = "A"
-- I = ["A"]
-- X = ["A", "B", "C"]
-- U = U - X = ["E", "F"]
-- InducedGraph g U = [("E", (["F"], 0)), ("F", (["E"], 0))]
inducedGraph :: Graph -> [Node] -> Graph
--inducedGraph g nodes =  Map.fromList ( map (\x -> (x, (adj x, 0))) nodes `using` parBuffer 100 rseq )
--inducedGraph g nodes =  Map.fromList ( map (\x -> (x, (adj x, 0))) nodes `using` parListChunk 100 rseq )
inducedGraph g nodes =  Map.fromList ( parMap rseq (\x -> (x, (adj x, 0))) nodes )
                            where adj = (\nx -> filter (\y -> y `elem` nodes) $ getNeighbors nx g)

-- g = fromList [("A",(["B","C"],0)),("B",(["A","C","D","E","F"],0)),
independentSet :: Graph -> Graph -> [Node] -> [Node] -> [Node]
independentSet _ _ [] i = i
independentSet g ig u@(x:_) i | length (Map.keys ig) == 0 = i
                               | otherwise = independentSet g ig_new u_new i_new
                                             where i_new = x : i
                                                   u_new = filter (\y -> y `notElem` (x: getNeighbors x g)) u
                                                   ig_new = inducedGraph g  u_new
                                                  
colorNodes :: Graph -> [Node] -> Color -> Graph
colorNodes g [] _ = g
colorNodes g nodes c = Map.union (fst pr) (Map.mapWithKey (\_ x -> (fst x, c)) (snd pr))
                       where pr = Map.partitionWithKey (\k _ -> k `notElem` nodes) g
--colorNodes g (x:xs) c = colorNodes (setColor g x c) xs c

colorIndependent :: Graph -> Graph -> [Node] -> [Color] -> Maybe Graph
colorIndependent g _ _ [] = Just g
colorIndependent g _ [] _ = Just g
colorIndependent g ig u (c:cs) | length (Map.keys ig) == 0 = Just g
                               | otherwise = runEval $ do 
                                            i_new <- rseq $ independentSet ig ig u_nodes [] 
                                            u_new <- rpar $ filter (\y -> y `notElem` i_new) u
                                            colored_g <- rpar $ colorNodes g i_new c
                                            ig_new <- rpar $ inducedGraph g u_new
                                            return $ colorIndependent colored_g ig_new u_new cs
                                            where u_nodes = Map.keys ig
