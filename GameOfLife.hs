{- *** QUESTION 2.1 *** -}
pretty :: [ [ [ Char ] ] ] -> [ Char ]
pretty = concatMap unlines


---------------------------------------------------------------------------------------------------------------

{- *** QUESTION 2.2 *** -}
type Point = ( Int, Int )

glider :: [ Point ]
glider = [(0,2),(1,3),(2,1),(2,2),(2,3)] 
  

visualisation :: Int -> Int -> [ [ Point ] ] -> [ [ [ Char ] ] ]
visualisation w h =  map (visualise w h)

visualise :: Int -> Int -> [ Point ] -> [ [ Char ] ]
visualise w h gen = map (\y -> map (\x -> hashDot (isAlive gen (x,y))) [0..w]) [0..h]

isAlive :: [ Point ] -> Point -> Bool
isAlive pts pt = pt ‘elem‘ pts

hashDot :: Bool -> Char
hashDot True = ’#’
hashDot False = ’.’


--------------------------------------------------------------------------------------------------------------

{- *** QUESTION 2.3 *** -}
evolution :: [ Point ] -> [ [ Point ] ]
evolution =  iterate evolve

evolve :: [ Point ] -> [ Point ]
evolve gen = nub (survivors gen ++ births gen)

survivors :: [ Point ] -> [ Point ]
survivors gen =  filter (survives gen) gen

survives :: [ Point ] -> Point -> Bool
survives gen pt = countLiveNeighbours gen pt ‘elem‘ [2,3]

births :: [ Point ] -> [ Point ]
births gen = concatMap (born gen) gen

born :: [ Point ] -> Point -> [ Point ]
born gen pt = filter (\qt -> countLiveNeighbours gen qt ‘elem‘ [3])(neighbours pt)

neighbours :: Point -> [ Point ]
neighbours (x,y) = [ (x-1, y-1), (x, y-1), (x+1, y-1) ,(x-1,y ), (x+1,y ) , (x-1, y+1), (x, y+1), (x+1, y+1) ]

liveNeighbours :: [ Point ] -> Point -> [ Point ]
liveNeighbours gen pt = filter (isAlive gen) (neighbours pt)

countLiveNeighbours :: [ Point ] -> Point -> Int
countLiveNeighbours gen pt = length (liveNeighbours gen pt)


---------------------------------------------------------------------------------------------------------------------------

-- ### MAIN FUNCTION ### 
main :: IO ()
main = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))
