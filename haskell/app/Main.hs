module Main where

-- import Debug.Trace
import qualified Data.Map as M

upperBound :: Integer
upperBound = (2 :: Integer) ^ (128 :: Integer)

-- Adjacency Matrix
-- n^2
adjacency :: (Int, Int, Int) -> Int -> [[Integer]]
adjacency (a, b, c) n = rows n n
  where
    -- nxn
    rows :: Int -> Int -> [[Integer]]
    rows 0 j = [row (a, b, c) 0 j]
    rows i j = row (a, b, c) i j : rows (i - 1) j

-- row in the adjacency matrix
-- n
row :: (Int, Int, Int) -> Int -> Int -> [Integer]
row (a, b, c) i 0 = [weight (a, b, c) i 0]
row (a, b, c) i j = weight (a, b, c) i j : row (a, b, c) i (j - 1)

-- weight calculation for edges i j
-- a bit weird with toInteger because of potential overflows
-- c
weight :: (Int, Int, Int) -> Int -> Int -> Integer
weight (a, b, c) i j = toInteger a ^ (abs (i - j) `mod` c) + toInteger (b * (i - j) ^ (2 :: Integer)) - 1

-- initial distances
-- n
distance :: Int -> Int -> [Integer]
distance myk myn = distanceToN myk myn 0
  where
    -- n 
    distanceToN :: Int -> Int -> Int -> [Integer]
    distanceToN k n acc
      | acc == n = [upperBound]
      | acc == (k - 1) = (0 :: Integer) : distanceToN k n (acc + 1)
      | otherwise = upperBound : distanceToN k n (acc + 1)

-- marks for visited vertices
-- n
visits :: Integer -> [Bool]
visits 0 = []
visits n = False : visits (n - 1)

-- my adjacency matrix
-- n^2
adjmat :: (Int, Int, Int) -> Int -> [[Integer]]
adjmat (a, b, c) = adjacency (a, b, c)

-- distances for adjacency matrix
-- n
dis :: Int -> Int -> [Integer]
dis k n = distance k (n - 1)

-- visit initialization
-- n
vis :: Int -> [Bool]
vis n = visits (toInteger n)

-- distancemap as final what the final output should be
-- n
distanceMap :: Int -> Int -> [(Integer, Bool)]
distanceMap k n = zip (dis k n) (vis n)

-- marks visited vertices as Nothing so we can ignore them later more easily
-- n
unvisited :: Int -> Int -> [(Maybe Integer, Bool)]
unvisited k n = map mapUnvisited (distanceMap k n)
  where
    -- c
    mapUnvisited :: (Integer, Bool) -> (Maybe Integer, Bool)
    mapUnvisited (dist, visited)
      | not visited = (Just dist, visited)
      | otherwise = (Nothing, visited)

-- sets list[index] as true
-- n
markAsVisited :: Int -> [(Maybe Integer, Bool)] -> [(Maybe Integer, Bool)]
markAsVisited index = markAsVisitedAcc index 0
  where
    -- n
    markAsVisitedAcc :: Int -> Int -> [(Maybe Integer, Bool)] -> [(Maybe Integer, Bool)]
    markAsVisitedAcc _ _ [] = []
    markAsVisitedAcc i acc (y : ys)
      | i == acc = (fst y, True) : markAsVisitedAcc i (acc + 1) ys
      | otherwise = y : markAsVisitedAcc i (acc + 1) ys

-- get index of lowest unvisited vertex
-- n
lowestIndex :: [(Maybe Integer, Bool)] -> Int
lowestIndex vertices = lowest vertices upperBound 0 0
  where
    -- n
    lowest :: [(Maybe Integer, Bool)] -> Integer -> Int -> Int -> Int
    lowest [] _ _ index = index
    lowest ((_, True) : xs) currentLowest iterator index = lowest xs currentLowest (iterator + 1) index
    lowest ((Nothing, _) : xs) currentLowest iterator index = lowest xs currentLowest (iterator + 1) index
    lowest ((Just x, _) : xs) currentLowest iterator index
      | x < currentLowest = lowest xs x (iterator + 1) iterator
      | otherwise = lowest xs currentLowest (iterator + 1) index

-- updates distances of current lowests neighbours
-- n
distanceFromLowest :: Int -> [Integer] -> [[Integer]] -> [Integer]
distanceFromLowest indexOfLowest currentDistances adjacencyMatrix = distanceUpdate 0 currentDistances
  where
    rowForLowest :: [Integer]
    rowForLowest = adjacencyMatrix !! indexOfLowest
    distanceOfLowest :: Integer
    distanceOfLowest = currentDistances !! indexOfLowest
    -- c
    distanceOfNeighbour :: Int -> Integer
    distanceOfNeighbour neighbourIndex = distanceOfLowest + (rowForLowest !! neighbourIndex)
    -- n
    distanceUpdate :: Int -> [Integer] -> [Integer]
    distanceUpdate acc oldDistances
      | acc == length oldDistances = []
      | distanceOfNeighbour acc < (oldDistances !! acc) = distanceOfNeighbour acc : distanceUpdate (acc + 1) oldDistances
      | otherwise = oldDistances !! acc : distanceUpdate (acc + 1) oldDistances

-- n
distanceUpdateForVisit :: [Integer] -> [(Maybe Integer, Bool)] -> [(Maybe Integer, Bool)]
distanceUpdateForVisit newdis = updateVisitedOnDistance 0
  where
    -- n
    updateVisitedOnDistance :: Int -> [(Maybe Integer, Bool)] -> [(Maybe Integer, Bool)]
    updateVisitedOnDistance _ [] = []
    updateVisitedOnDistance acc ((Nothing, oth) : xs) = (Nothing, oth) : updateVisitedOnDistance (acc + 1) xs
    updateVisitedOnDistance acc ((Just x, oth) : xs)
      | newdis !! acc < x = (Just (newdis !! acc), oth) : updateVisitedOnDistance (acc + 1) xs
      | otherwise = (Just x, oth) : updateVisitedOnDistance (acc + 1) xs

-- max n iteracija
-- n
dijkstra :: Int -> Int -> Int -> [(Maybe Integer, Bool)] -> [[Integer]] -> [Integer] -> [Integer]
dijkstra l kill acc unvisitedVertices adjacencyMatrix distanceVector
  | acc >= kill = distanceVector
  | otherwise = do
      -- n 
      let lowest = lowestIndex unvisitedVertices
      if lowest == (l - 1)
        then distanceVector
        else do
          -- n
          let visitVertex = markAsVisited lowest unvisitedVertices
          -- n
          let newDistances = distanceFromLowest lowest distanceVector adjacencyMatrix
          -- n
          let updatedUnvisited = distanceUpdateForVisit newDistances visitVertex
          dijkstra l kill (acc + 1) updatedUnvisited adjacencyMatrix newDistances

inputPrompt :: String -> IO Int
inputPrompt var = do
  putStrLn ("Unesite prirodan broj " ++ var ++ ": ")
  input <- getLine
  pure (read input :: Int)

main :: IO ()
main = do
  n <- inputPrompt "n"
  a <- inputPrompt "a"
  b <- inputPrompt "b"
  c <- inputPrompt "c"
  k <- inputPrompt "k"
  l <- inputPrompt "l"
  putStrLn ("Udaljenost izmedu k i l je " ++ show (dijkstra l n 0 (unvisited k n) (adjmat (a, b, c) n) (dis k n) !! (l - 1)))
