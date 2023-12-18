module Main where

num :: Integer
num = 7

a :: Integer
a = 8

b :: Integer
b = 1

c :: Integer
c = 2

k :: Integer
k = 1

l :: Integer
l = 6

-- Adjacency Matrix
adjacency :: Integer -> [[Integer]]
adjacency n = rows n n
    where
        rows :: Integer -> Integer -> [[Integer]]
        rows 0 j = [row 0 j]
        rows i j = row i j : rows (i - 1) j

-- row in the adjacency matrix
row :: Integer -> Integer -> [Integer]
row i 0 = [weight i 0]
row i j = weight i j : row i (j - 1)

-- weight calculation for edges i j
weight :: Integer -> Integer -> Integer
weight i j = a ^ ((i - j) `mod` c) + b * (i - j) ^ (2 :: Integer) - 1 

-- initial distances
distance :: Integer -> [Integer]
distance (-1) = []
distance n
    | n == (k - 1) = (0 :: Integer) : distance (n - 1)
    | otherwise = ((2 :: Integer) ^ (64 :: Integer)) : distance (n - 1)

-- marks for visited vertices
visits :: Integer -> [Bool]
visits (-1) = []
visits n = False : visits (n - 1)

-- 1 check unvisited vertices
-- 2 find the vertex with the lowestIndex distance away
-- 3 if its the vertex l then its done
-- 4 position youself on it
-- 5 check its neighbours (row = vertex id) and update their distances if needed
-- go to 1

-- my adjacency matrix
adjmat :: [[Integer]]
adjmat = adjacency (num - 1)

-- distances for adjacency matrix
dis :: [Integer]
dis = reverse $ distance (num - 1)

-- visit initialization
vis :: [Bool]
vis = visits (num - 1)

-- distancemap as final what the final output should be
distanceMap :: [(Integer, Bool)]
distanceMap = zip dis vis

-- sets element k=0 as visited
-- distanceMap = map setToTrue $ zip dis vis
--     where
--         setToTrue :: (Integer, Bool) -> (Integer, Bool)
--         setToTrue (x, y)
--             | x == 0 = (x, True)
--             | otherwise = (x, y)

-- marks visited vertices as Nothing so we can ignore them later more easily 
unvisited :: [(Maybe Integer, Bool)]
unvisited = map mapUnvisited distanceMap 
    where
        mapUnvisited :: (Integer, Bool) -> (Maybe Integer, Bool)
        mapUnvisited (dist, visited)
            | visited == False = (Just dist, visited)
            | otherwise = (Nothing, visited)

-- returns first just from list (this should never get to [] -> if it does then L is literally the final element)
-- check edge case later!
firstJust :: [Maybe Integer] -> Integer
firstJust [] = error "empty list"
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : _) = x

-- returns the index of the element with the lowest distance 
lowestIndex :: Integer
lowestIndex = lowest (fstX unvisited) (firstJust $ fstX unvisited) 0 0
    where
        -- maps just the integers from unvisited
        fstX :: [(Maybe Integer, Bool)] -> [Maybe Integer]
        fstX xs = map (\x -> fst x) xs
        -- helper function which does everything
        lowest :: [Maybe Integer] -> Integer -> Integer -> Integer -> Integer
        lowest [] _ _ index = index
        lowest (Nothing : xs) lowest iterator index = lowest xs lowest iterator index
        lowest (Just x : xs) lowest iterator index
            | x < lowest = lowest xs x (iterator + 1) iterator 
            | otherwise = lowest xs lowest (iterator + 1) index





main :: IO ()
main = putStrLn $ show $ unvisited


