-- This function converts a 3D list of characters into a single string by concatenating
-- the lines of each 2D list together.
import Data.List (nub)

pretty :: [[[Char]]] -> [Char]
pretty = concatMap unlines


-- Define a type synonym for a Point, which is a tuple of two integers.
type Point = (Int, Int)

-- Initial configuration of a glider pattern in the Game of Life.
glider :: [Point]
glider = [(0,2),(1,3),(2,1),(2,2),(2,3)] 
  

-- Function to visualize the Game of Life grid over multiple generations.
visualisation :: Int -> Int -> [[Point]] -> [[[Char]]]
visualisation w h = map (visualise w h)

-- Function to visualize a single generation of the Game of Life grid.
visualise :: Int -> Int -> [Point] -> [[Char]]
visualise w h gen = map (\y -> map (\x -> hashDot (isAlive gen (x,y))) [0..w]) [0..h]

-- Check if a given point is alive (exists in the list of alive points).
isAlive :: [Point] -> Point -> Bool
isAlive pts pt = pt `elem` pts

-- Convert a boolean (alive/dead) to a character ('#' for alive, '.' for dead).
hashDot :: Bool -> Char
hashDot True = '#'
hashDot False = '.'


-- Function to generate successive generations of the Game of Life.
evolution :: [Point] -> [[Point]]
evolution = iterate evolve

-- Function to evolve a single generation of the Game of Life grid.
evolve :: [Point] -> [Point]
evolve gen = nub (survivors gen ++ births gen)

-- Function to filter out surviving points from the current generation.
survivors :: [Point] -> [Point]
survivors gen = filter (survives gen) gen

-- Check if a point survives to the next generation based on the number of live neighbors.
survives :: [Point] -> Point -> Bool
survives gen pt = countLiveNeighbours gen pt `elem` [2,3]

-- Function to generate new points (births) in the next generation.
births :: [Point] -> [Point]
births gen = concatMap (born gen) gen

-- Function to generate new points (births) around a given point.
born :: [Point] -> Point -> [Point]
born gen pt = filter (\qt -> countLiveNeighbours gen qt `elem` [3]) (neighbours pt)

-- List of neighboring points around a given point (including diagonals).
neighbours :: Point -> [Point]
neighbours (x,y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

-- List of live neighboring points around a given point.
liveNeighbours :: [Point] -> Point -> [Point]
liveNeighbours gen pt = filter (isAlive gen) (neighbours pt)

-- Count the number of live neighbors around a given point.
countLiveNeighbours :: [Point] -> Point -> Int
countLiveNeighbours gen pt = length (liveNeighbours gen pt)



-- ### MAIN FUNCTION ### 
-- Entry point of the program: prints the visual representation of the Game of Life for the initial glider pattern.
main :: IO ()
main = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))
