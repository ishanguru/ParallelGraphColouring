module GraphColoringAlgo
( backtracking,
  colorIndependent,
  divideConquerPar,
  greedy
) where

import Utils
import Data.List (sort)
import qualified Data.Map as Map
import Control.Parallel.Strategies (rpar, rseq, runEval, parListChunk, using)
import System.Random.Shuffle

-- assigns a colour to each node in the graph
-- e.g. backtracking (Map.keys g) [1,2,3,4] g
backtracking :: [Node] -> [Color] -> [Color] -> Graph -> Maybe Graph
backtracking _ [] _ g = Just g
backtracking [] _ _ g = Just g
backtracking _ _ [] _ = Nothing
backtracking nodes@(n:ns) colors (c:cs) g
      | validColor n g c = case (backtracking ns colors colors $ setColor g n c) of
                              Just gout -> Just gout
                              Nothing -> backtracking nodes colors cs g
      | otherwise = backtracking nodes colors cs g

greedy :: [Node] -> [Color] -> [Color] -> Graph -> Maybe Graph
greedy _ [] _ g = Just g
greedy [] _ _ g = Just g
greedy _ _ [] _ = Nothing
greedy nodes@(n:ns) colors (c:cs) g
      | validColor n g c = greedy ns colors colors $ setColor g n c
      | otherwise = greedy nodes colors cs g

-- following algo in:
-- http://www.ii.uib.no/~assefaw/pub/coloring/thesis.pdf
-- an order of mag slower than backtracking algorithm
-- g = fromList [("A",(["B","C"],0)),("B",(["A","C","D","E","F"],0)),
-- ("C",(["A","B","D"],0)),("D",(["B","C","E"],0)),("E",(["B","D","F"],0)),("F",(["B","E"],0))]
-- U = ["A","B","C","D","E","F"]
-- v = "A"
-- I = ["A"]
-- X = ["A", "B", "C"]
-- U = U - X = ["E", "F"]
-- InducedGraph g U = [("E", (["F"], 0)), ("F", (["E"], 0))]
inducedGraph :: Graph -> [Node] -> Graph
inducedGraph g nodes =  Map.fromList ( map (\x -> (x, (adj x, 0))) nodes `using` parListChunk (length nodes `div` 2) rseq )
                            where adj = (\nx -> filter (\y -> y `elem` nodes) $ getNeighbors nx g)

-- g = fromList [("A",(["B","C"],0)),("B",(["A","C","D","E","F"],0)),
independentSet :: Graph -> Graph -> [Node] -> [Node] -> [Node]
independentSet _ _ [] i = i
independentSet g ig nodes i | length (Map.keys ig) == 0 = i
                            | otherwise = independentSet g ig_new u_new i_new
                                           where x = head nodes
                                                 i_new = x : i
                                                 u_new = filter (\y -> y `notElem` (x: getNeighbors x g)) nodes
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

divideConquerPar :: [Node] -> Graph -> Maybe Graph
divideConquerPar n g = divideConquerPar' n [1..(length (Map.keys g))] g

divideConquerPar' :: [Node] -> [Color] -> Graph -> Maybe Graph
divideConquerPar' _ [] g = Just g
divideConquerPar' [] _ g = Just g
divideConquerPar' [n] colors g
  | allVerticesColored g = Just g
  | otherwise = 
      if nodeColor > 0 then do
          Just $ setColor g n nodeColor
      else do
          Nothing
      where nodeColor = colorNode n colors g
divideConquerPar' nodes colors g
  | allVerticesColored g = Just g
  | otherwise = runEval $ do
    front <- rpar $ divideConquerPar' first colors $ subGraph first g Map.empty
    back <- rpar $ divideConquerPar' second colors $ subGraph second g Map.empty
    case (front, back) of 
      (Just g, Nothing) -> return $ Just $ g
      (Nothing, Just g) -> return $ Just $ g
      (Just g, Just y)  -> return $ Just $ merge (Map.keys (Map.union g y)) colors $ Map.union g y
      _                 -> return $ Nothing
    where (first, second) = splitAt (length nodes `div` 2) nodes

merge :: [Node] -> [Color] -> Graph -> Graph
merge [] _ g = g
merge [x] colors g = setColors g (findClashingNodes x g) $ head updateColors 
  where updateColors = filter (validColor x g) colors
merge (x:xs) colors g = merge xs updateColors $ setColors g (findClashingNodes x g) $ head updateColors
  where updateColors = filter (validColor x g) colors

subGraph :: [Node] -> Graph -> Graph -> Graph 
subGraph [] _ x = x
subGraph [n] g x = Map.union x (Map.insert n ((getNeighbors n g), (getColor n g)) x)
subGraph (n:ns) g x = subGraph ns g (Map.union x (Map.insert n ((getNeighbors n g), (getColor n g)) x))

