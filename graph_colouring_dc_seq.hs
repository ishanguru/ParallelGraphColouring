import qualified Data.Map as Map
import System.Exit(die)
import System.IO(Handle, hIsEOF, hGetLine, withFile, IOMode(ReadMode))
import System.Environment(getArgs, getProgName)
import Control.Parallel.Strategies
import qualified Data.List as List

type Node = String
type Color = Int
type AdjList = [Node]
type Graph = Map.Map(Node) (AdjList, Color)

-- stack ghc -- --make -Wall -O graph_colouring.hs
main :: IO ()
main = do
  args <- getArgs
  case args of
    [graph_file, number_colours] -> do
      let colours = read number_colours
      g <- readGraphFile graph_file
      let output = colorGraph (Map.keys g) [1..colours] g
      print $ maximum $ getColors (Map.keys output) output
      let result = isValidGraph $ output

      if result then do 
        putStrLn "Successfully coloured graph"
        writeFile "out" ("true\n")
      else do 
        putStrLn "Unable to colour graph"
        writeFile "out" ("false\n")

    _ -> do 
        pn <- getProgName
        die $ "Usage: " ++ pn ++ " <graph-filename> <number-of-colors>"

readGraphLine :: Handle -> Graph -> IO Graph
readGraphLine handle g = do args  <- (wordsWhen (==':')) <$> hGetLine handle
                            case args of
                              [node, adj] -> return $ Map.insert node (readAdjList adj, 0) g
                              _           -> return g                          

readGraphFile :: String -> IO Graph
readGraphFile filename  = withFile filename ReadMode $ \handle -> loop handle readGraphLine Map.empty

loop :: Handle -> (Handle -> Graph -> IO Graph) -> Graph -> IO Graph
loop h f g = do 
              outgraph <- f h g
              eof <- hIsEOF h
              if eof then do return outgraph
              else do (loop h f outgraph) >>= (\y -> return y)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                         where (w, s'') = break p s'

readAdjList :: String ->  AdjList
readAdjList x = wordsWhen (==',') x

getColor :: Node -> Graph -> Color
getColor n g = case Map.lookup n g of
                 Just v -> (snd v)
                 Nothing -> 0

getColors :: [Node] -> Graph -> [Color]
getColors [] _ = []
getColors (x:xs) g = getColor x g : getColors xs g

getNeighbors :: Node -> Graph -> AdjList
getNeighbors n g = case Map.lookup n g of
                 Just v -> (fst v)
                 Nothing -> []

setColor :: Graph -> Node -> Color -> Graph
setColor g n c = case Map.lookup n g of
                  Just v -> Map.insert n ((fst v), c) g
                  Nothing -> g

setColors :: Graph -> [Node] -> Color -> Graph
setColors g [] _ = g
setColors g [n] c = setColor g n c
setColors g (n:ns) c = setColors (setColor g n c) ns c

colorGraph :: [Node] -> [Color] -> Graph -> Graph
colorGraph _ [] g = g
colorGraph [] _ g = g
colorGraph [n] colors g
  | allVerticesColored g = g
  | otherwise = 
      if nodeColor > 0 then do
          setColor g n nodeColor
      else do
          error "can't color graph"
      where nodeColor = colorNode n colors g
colorGraph nodes colors g
  | allVerticesColored g = g
  | otherwise = do
    let front =  colorGraph first colors $ subGraph first g Map.empty
    let back = colorGraph second colors $ subGraph second g Map.empty
    let join  = Map.union front back
    merge (Map.keys join) colors join
    where first = take ((length nodes) `div` 2) nodes
          second = drop ((length nodes) `div` 2) nodes

merge :: [Node] -> [Color] -> Graph -> Graph
merge [] _ g = g
merge [x] colors g = setColors g (findClashingNodes x g) $ head updateColors 
  where updateColors = filter (validColor x g) colors
merge (x:xs) colors g = merge xs updateColors $ setColors g (findClashingNodes x g) $ head updateColors
  where updateColors = filter (validColor x g) colors

findClashingNodes :: Node -> Graph -> [Node]
findClashingNodes n g = [ x | x <- (getNeighbors n g), (getColor n g) == (getColor x g) ]

subGraph :: [Node] -> Graph -> Graph -> Graph 
subGraph [] _ x = x
subGraph [n] g x = Map.union x (Map.insert n ((getNeighbors n g), (getColor n g)) x)
subGraph (n:ns) g x = subGraph ns g (Map.union x (Map.insert n ((getNeighbors n g), (getColor n g)) x))

colorNode :: Node -> [Color] -> Graph -> Color
colorNode n c g | ncolors == length c = 0
                | otherwise = ncolors + 1
                where ncolors = length $ colorNode' n c g

colorNode' :: Node -> [Color] -> Graph -> [Bool]
colorNode' n c g = takeWhile (\x -> not x) [ x | x <- map (validColor n g) c]

allVerticesColored :: Graph -> Bool
allVerticesColored g = 0 `notElem` getColors (Map.keys g) g

isValidGraph :: Graph -> Bool
isValidGraph g = isValidGraph' (Map.keys g) g

isValidGraph' :: [Node] -> Graph -> Bool
isValidGraph' [] _ = False
isValidGraph' [n] g = getColor n g >= 0
isValidGraph' (n:ns) g 
    | getColor n g `notElem` getColors (getNeighbors n g) g = isValidGraph' ns g
    | otherwise = False

validColor :: Node -> Graph -> Color -> Bool
validColor n g c = c `notElem` getColors (getNeighbors n g) g


