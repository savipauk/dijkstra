module Main where

import Debug.Trace

-- Adjacency Matrix
adjacency :: (Int, Int, Int) -> Integer -> [[Integer]]
adjacency (a, b, c) n = rows n n
  where
    rows :: Integer -> Integer -> [[Integer]]
    rows 0 j = [row (a, b, c) 0 j]
    rows i j = row (a, b, c) i j : rows (i - 1) j

-- row in the adjacency matrix
row :: (Int, Int, Int) -> Integer -> Integer -> [Integer]
row (a, b, c) i 0 = [weight (a, b, c) i 0]
row (a, b, c) i j = weight (a, b, c) i j : row (a, b, c) i (j - 1)

-- weight calculation for edges i j
weight :: (Int, Int, Int) -> Integer -> Integer -> Integer
weight (a, b, c) i j = toInteger a ^ ((i - j) `mod` toInteger c) + toInteger b * (i - j) ^ (2 :: Integer) - 1

-- initial distances
-- TODO: make this from 0 to n
distance :: Int -> Integer -> [Integer]
distance _ (-1) = []
distance k n
  | n == (toInteger k - 1) = (0 :: Integer) : distance k (n - 1)
  | otherwise = ((2 :: Integer) ^ (64 :: Integer)) : distance k (n - 1)

-- marks for visited vertices
visits :: Integer -> [Bool]
visits 0 = []
visits n = False : visits (n - 1)

-- my adjacency matrix
adjmat :: (Int, Int, Int) -> Int -> [[Integer]]
adjmat (a, b, c) n = adjacency (a, b, c) (toInteger n)

-- distances for adjacency matrix
dis :: Int -> Int -> [Integer]
dis k n = reverse $ distance k (toInteger n - 1)

-- visit initialization
vis :: Int -> [Bool]
vis n = visits (toInteger n)

-- distancemap as final what the final output should be
distanceMap :: Int -> Int -> [(Integer, Bool)]
distanceMap k n = zip (dis k n) (vis n)

-- sets element k=0 as visited
-- distanceMap = map setToTrue $ zip dis vis
--     where
--         setToTrue :: (Integer, Bool) -> (Integer, Bool)
--         setToTrue (x, y)
--             | x == 0 = (x, True)
--             | otherwise = (x, y)

-- marks visited vertices as Nothing so we can ignore them later more easily
unvisited :: Int -> Int -> [(Maybe Integer, Bool)]
unvisited k n = map mapUnvisited (distanceMap k n)
  where
    mapUnvisited :: (Integer, Bool) -> (Maybe Integer, Bool)
    mapUnvisited (dist, visited)
      | not visited = (Just dist, visited)
      | otherwise = (Nothing, visited)

-- sets list[index] as true
markAsVisited :: Int -> [(Maybe Integer, Bool)] -> [(Maybe Integer, Bool)]
markAsVisited index = markAsVisitedAcc index 0
  where
    markAsVisitedAcc :: Int -> Int -> [(Maybe Integer, Bool)] -> [(Maybe Integer, Bool)]
    markAsVisitedAcc _ _ [] = []
    markAsVisitedAcc i acc (y : ys)
      | i == acc = (fst y, True) : markAsVisitedAcc i (acc + 1) ys
      | otherwise = y : markAsVisitedAcc i (acc + 1) ys

-- returns first just from list (this should never get to [] -> if it does then L is literally the final element)
-- check edge case later!
firstJust :: [Maybe Integer] -> Integer
firstJust [] = error "empty list"
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : _) = trace ("x: " ++ show x) x

firstJustIndex :: [Maybe Integer] -> Int -> Int
firstJustIndex [] _ = error "empty list"
firstJustIndex (Nothing : xs) acc = firstJustIndex xs (acc + 1)
firstJustIndex (Just _ : _) acc = acc

-- returns the index of the element with the lowest distance
lowestIndex :: [(Maybe Integer, Bool)] -> Int
lowestIndex vertices = lowest (fstX vertices) ((2 :: Integer) ^ (64 :: Integer)) 0 0
  where
    -- maps just the integers from unvisited
    fstX :: [(Maybe Integer, Bool)] -> [Maybe Integer]
    fstX = map (\x -> if snd x then Nothing else fst x)
    -- helper function which does everything
    lowest :: [Maybe Integer] -> Integer -> Int -> Int -> Int
    lowest [] _ _ index = index
    lowest (Nothing : xs) currentLowest iterator index = lowest xs currentLowest (iterator + 1) index
    lowest (Just x : xs) currentLowest iterator index
      | x < currentLowest = lowest xs x (iterator + 1) iterator
      | otherwise = lowest xs currentLowest (iterator + 1) index

-- lowestIndex :: [(Maybe Integer, Bool)] -> Int
-- lowestIndex vertices =
--   trace ("Initial vertices: " ++ show vertices) $
--     -- lowest (fstX vertices) (firstJust $ fstX vertices) (firstJustIndex (fstX vertices) 0) 0
--     lowest (fstX vertices) ((2 :: Integer) ^ (64 :: Integer)) 0 0
--   where
--     -- maps just the integers from unvisited
--     fstX :: [(Maybe Integer, Bool)] -> [Maybe Integer]
--     fstX = map (\x -> if snd x then Nothing else fst x)
--
--     -- helper function which does everything
--     lowest :: [Maybe Integer] -> Integer -> Int -> Int -> Int
--     lowest [] _ _ index = trace ("Final index: " ++ show index) index
--     lowest (Nothing : xs) currentLowest iterator index =
--       trace ("Skipping Nothing at position " ++ show iterator) $
--         lowest xs currentLowest (iterator + 1) index
--     lowest (Just x : xs) currentLowest iterator index
--       | x < currentLowest =
--           trace ("Found a new lowest value at position " ++ show iterator ++ ": " ++ show x) $
--             lowest xs x (iterator + 1) iterator
--       | otherwise =
--           trace ("Skipping value at position " ++ show iterator ++ ": " ++ show x ++ ", because current lowest is: " ++ show currentLowest) $
--             lowest xs currentLowest (iterator + 1) index
--
-- set lowest as visited
-- go again
-- if lowest == l end

-- updates distances of current lowests neighbours
distanceFromLowest :: Int -> [Integer] -> [[Integer]] -> [Integer]
distanceFromLowest indexOfLowest currentDistances adjacencyMatrix = distanceUpdate 0 currentDistances
  where
    rowForLowest :: [Integer]
    rowForLowest = adjacencyMatrix !! indexOfLowest
    distanceOfLowest :: Integer
    distanceOfLowest = currentDistances !! indexOfLowest
    distanceOfNeighbour :: Int -> Integer
    distanceOfNeighbour neighbourIndex = distanceOfLowest + (rowForLowest !! neighbourIndex)
    distanceUpdate :: Int -> [Integer] -> [Integer]
    distanceUpdate acc oldDistances
      | acc == length oldDistances = []
      | distanceOfNeighbour acc < (oldDistances !! acc) = distanceOfNeighbour acc : distanceUpdate (acc + 1) oldDistances
      | otherwise = oldDistances !! acc : distanceUpdate (acc + 1) oldDistances

-- novedistance = distanceFromLowest (lowestIndex unvisited) dis adjmat
-- uses lowestIndex
-- update unvisited
-- if lowestIndex == l => end
-- go again with novedistance

-- 1 check unvisited vertices
-- 2 find the vertex with the lowestIndex distance away
-- 3 if its the vertex l then its done
-- 4 position youself on it
-- 5 check its neighbours (row = vertex id) and update their distances if needed
-- go to 1

-- test :: [Integer]
-- test = distanceFromLowest (markAsVisited (lowestIndex unvisited) unvisited) dis adjmat
-- test = do
--     let lowest = lowestIndex unvisited
--     newUnvisited <- markAsVisited unvisited lowest
--     distanceFromLowest lowest dis adjmat

distanceUpdateForVisit :: [Integer] -> [(Maybe Integer, Bool)] -> [(Maybe Integer, Bool)]
distanceUpdateForVisit newdis = m 0
  where
    m :: Int -> [(Maybe Integer, Bool)] -> [(Maybe Integer, Bool)]
    m _ [] = []
    m acc ((Nothing, oth) : xs) = (Nothing, oth) : m (acc + 1) xs
    m acc ((Just x, oth) : xs)
      | newdis !! acc < x = (Just (newdis !! acc), oth) : m (acc + 1) xs
      | otherwise = (Just x, oth) : m (acc + 1) xs

-- max n iteracija
dijkstra :: Int -> Int -> Int -> [(Maybe Integer, Bool)] -> [[Integer]] -> [Integer] -> [Integer]
dijkstra l kill acc unvisitedVertices adjacencyMatrix distanceVector
  | acc >= kill = distanceVector
  | otherwise = do
      let lowest = lowestIndex unvisitedVertices
      if lowest == (l - 1)
        then distanceVector
        else do
          let visitVertex = markAsVisited lowest unvisitedVertices
          let newDistances = distanceFromLowest lowest distanceVector adjacencyMatrix
          let updatedUnvisited = distanceUpdateForVisit newDistances visitVertex
          -- trace ("Step " ++ show acc ++ ": " ++ show lowest ++ "\n" ++ show updatedUnvisited ++ "\n" ++ show newDistances ++ "\n" ++ "distance: " ++ show distanceVector ++ "\n") $
          dijkstra l kill (acc + 1) updatedUnvisited adjacencyMatrix newDistances

inputPrompt :: String -> IO Int
inputPrompt var = do
  putStrLn ("Unesite prirodan broj " ++ var ++ ": ")
  input <- getLine
  pure (read input :: Int)

main :: IO ()
-- main = print $ dijkstra num 0 unvisited adjmat dis !! (l - 1)
main = do
  n <- inputPrompt "n"
  a <- inputPrompt "a"
  b <- inputPrompt "b"
  c <- inputPrompt "c"
  k <- inputPrompt "k"
  l <- inputPrompt "l"
  let d = dijkstra l n 0 (unvisited k n) (adjmat (a, b, c) n) (dis k n)
  -- print d
  putStrLn ("Udaljenost izmedu k i l je " ++ show (d !! (l - 1)))
