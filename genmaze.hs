import Data.List
import System.Random
import System.Environment
import Control.Exception
import Text.Read
import System.Directory

---- The maze datatype

data Tile = Wall | Floor 

instance Show Tile where
    show Wall = "#"
    show Floor = " "

data Maze = Maze [[Tile]] 

instance Show Maze where
    show (Maze tiles) = 
        let
            shown = map (map show) tiles
            joined = map concat shown
        in
            intercalate "\n" joined

---- Functions for manipulating mazes

width :: Maze -> Int
width (Maze tiles) = if null tiles then 0 else length . head $ tiles

height :: Maze -> Int
height (Maze tiles) = length tiles


-- Create a new maze by placing maze 1 above maze 2
join_vert :: Maze -> Maze -> Maze
join_vert (Maze tiles1) (Maze tiles2) 
    | width (Maze tiles1) /= width (Maze tiles2) = error $ "Mazes have incorrect sizes (join_vert): " ++ (show . width $ Maze tiles1) ++ " vs " ++ (show . width $ Maze tiles2)
    | otherwise = Maze (tiles1 ++ tiles2)
    

-- Create a new maze by placing maze 1 to the left of maze 2
join_horiz :: Maze -> Maze -> Maze
join_horiz (Maze tiles1) (Maze tiles2)
    | height (Maze tiles1) /= height (Maze tiles2) = error "Mazes have incorrect sizes (join_horiz)"
    | otherwise = Maze (zipWith (++) tiles1 tiles2)


-- One tile wide walls
vert_wall :: Int -> Tile -> Maze
vert_wall length symb = Maze (replicate length [symb])

horiz_wall :: Int -> Tile -> Maze
horiz_wall length symb = Maze [(replicate length symb)]


-- One tile wide walls with holes in them
vert_wall_with_hole :: Int -> Int -> Maze
vert_wall_with_hole length hole
    | hole < 0 || hole >= length = error "Hole location invalid"
    | otherwise = 
        let 
            init = replicate hole [Wall]
            rest = replicate (length - hole - 1) [Wall]
        in
            Maze (init ++ [[Floor]] ++ rest)

horiz_wall_with_hole length hole =
    let
        Maze vert_wall = vert_wall_with_hole length hole
    in
        Maze (transpose vert_wall)

-- This function creates the "inner maze" (without the outer border)
-- Recursive idea:
--  1. Split the space into two by a single randomly chosen line.
--  2. Recursively generate two mazes, one for each side of the line
--  3. Draw a wall along the line, with exactly one hole in it
-- The resulting maze is guaranteed to be a tree
genmaze_helper :: StdGen -> Int -> Int -> (Maze, StdGen)
genmaze_helper gen h w 
    | even h || even w = error "h and w should be odd"
    | h == 1 = (horiz_wall w Floor, gen)
    | w == 1 = (vert_wall h Floor, gen)
    | otherwise = 
        let 
            -- randomly choose the dimension to split on
            -- (biased towards the dimension that has the most choices)
            total_choices = h `div` 2 + w `div` 2
            (r, gen1) = randomR (1, total_choices) gen
            x = if r > h `div` 2 then False else True

            -- s1 is the dimension to split on, s2 is the other dimension
            (s1, s2) = if x then (h, w) else (w, h)

            -- choose the split point
            (y, gen2) = randomR (1, s1 `div` 2) gen1
            split = 2 * y

            -- recursive call helper (flips dimensions if needed)
            recurse = (\ g (s1, s2) x -> 
                       if x 
                       then genmaze_helper g s1 s2
                       else genmaze_helper g s2 s1)

            -- Make two recursive mazes recursively
            (m1, gen3) = recurse gen2 (split-1, s2) x
            (m2, gen4) = recurse gen3 (s1-split, s2) x

            -- Create a joining wall that has exactly 1 hole
            (hole_rng, gen5) = randomR (0, (s2-1) `div` 2) gen4
            hole = hole_rng * 2 -- even number between 0 and (s2 - 1)

            wall_f = if x then horiz_wall_with_hole else vert_wall_with_hole
            join = wall_f s2 hole
            
            -- Join them together along the correct axis
            join_f = if x then join_vert else join_horiz
            answer = m1 `join_f` join `join_f` m2
        in
            (answer, gen5)

-- This function gets the inner maze from the helper, and then adds the border
genmaze :: StdGen -> String -> String -> Maze
genmaze gen hstring wstring =
    let
        -- Parse the arguments
        hparse = readMaybe hstring :: Maybe Int
        wparse = readMaybe wstring :: Maybe Int

        (h, w) = case (hparse, wparse) of
                      (Just x, Just y) -> (2*x+1, 2*y+1)
                      (Nothing, _)       -> error "Height is not a number"
                      (_, Nothing)       -> error "Width is not a number"

        -- Get the maze
        (m, _) = genmaze_helper gen (h-2) (w-2)

        -- Add the frame
        h_wall = horiz_wall (w-2) Wall
        v_wall = vert_wall h Wall

        with_top = h_wall `join_vert` m `join_vert` h_wall
        with_sides = v_wall `join_horiz` with_top `join_horiz` v_wall

    in
        with_sides


main = do
    args <- getArgs
    gen <- getStdGen

    if length args /= 3 
    then putStrLn "Usage: genmaze [height] [width] [filename]"
    else do
        let fname = args !! 2
        exists <- doesFileExist fname
        if exists
        then putStrLn ("File " ++ fname ++ " alread exists!")
        else do
            let maze = show (genmaze gen (args !! 0) (args !! 1))
            putStrLn maze
            writeFile fname maze
            putStrLn ("Maze written to " ++ fname)

