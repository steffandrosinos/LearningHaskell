import System.Environment
import System.IO
import Data.List

maze_path = "{{path}}\\maze2.txt"
maze_path2 = "{{path}}\\maze3.txt"

-- Useful code from Lecture 25
-- You may use this freely in your solutions

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

---- Part A
-- Question 1
get_maze :: String -> IO [String]
get_maze "" = error "No file input"
get_maze file = do 
                content <- readFile file            
                let contentLines = lines content    
                return contentLines                 
 
-- Question 2
print_maze :: [String] -> IO ()
print_maze [] = error "Incorrect input" 
print_maze m = do putStrLn (unlines (m))
                  
-- Question 3
is_wall :: [String] -> (Int, Int) -> Bool
is_wall [] (x, y) = error "Incorrect input"  
is_wall m (x, y) = if ((get m x y) == '#') then True else False

-- Question 4
place_player :: [String] -> (Int, Int) -> [String]
place_player [] (x, y) = error "Incorrect input"  
place_player m (x, y) = set m x y '@'

---- Part B
-- Question 5
move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) c 
    | c == 'w' = (x, (y-1))
    | c == 's' = (x, (y+1))
    | c == 'a' = ((x-1), y)
    | c == 'd' = ((x+1), y)
    | otherwise = (x, y)

-- Question 6
can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move m (-1, y) c = error "Negative index"
can_move m (x, -1) c = error "Negative index"
can_move m (x, y) c 
    | is_wall m (move (x, y) c) == True = False
    | is_wall m (move (x, y) c) == False = True

-- Question 7
game_loop :: [String] -> (Int, Int) -> IO ()
game_loop [] (x, y) = error "No maze inputted"
game_loop m (x, y) = do
                     let out = if (is_wall m (x, y) == False) then place_player m (x, y) else m
                     print_maze out
                     repeat_game_loop m (x, y)

repeat_game_loop m (x, y) = do
                            s <- getLine -- Using getChar here would result in "print_maze out" being run twice for some reason
                            putStrLn ("-")
                            let c = (s !! 0)
                                out = (if (can_move m (x, y) c == True) 
                                       then place_player m (move (x, y) c) 
                                       else place_player m (x, y))
                            print_maze out
                            repeat_game_loop m (if (can_move m (x, y) c == True)
                                                then (move (x, y) c)
                                                else (x,y))

---- Part C
-- Question 8
get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path [] (x, y) (a, b) = error "No maze inputted" 
get_path m (x, y) (a, b) = list_delete_repeat (maze_solver m (x, y) (a, b) [] []) 0

maze_solver m (x, y) (a, b) frontier visited
    | x == a && y == b = [(x, y)]
    | (can_move m (x, y) 'w' == True) && (((move (x, y) 'w') `elem` visited) == False) = [(x, y)] ++ maze_solver m (move (x, y) 'w') (a, b) (path_straight m (x, y) frontier) (add_visited (x, y) visited) 
    | (can_move m (x, y) 'd' == True) && (((move (x, y) 'd') `elem` visited) == False) = [(x, y)] ++ maze_solver m (move (x, y) 'd') (a, b) (path_straight m (x, y) frontier) (add_visited (x, y) visited) 
    | (can_move m (x, y) 's' == True) && (((move (x, y) 's') `elem` visited) == False) = [(x, y)] ++ maze_solver m (move (x, y) 's') (a, b) (path_straight m (x, y) frontier) (add_visited (x, y) visited) 
    | (can_move m (x, y) 'a' == True) && (((move (x, y) 'a') `elem` visited) == False) = [(x, y)] ++ maze_solver m (move (x, y) 'a') (a, b) (path_straight m (x, y) frontier) (add_visited (x, y) visited)
    | otherwise = maze_solver m (last frontier) (a, b) (take ((length frontier)-1) frontier) visited

add_visited (x, y) visited = visited ++ [(x, y)]
    
frontier_check m (x, y) frontier = if ((path_check m (x, y)) == True)
                                   then frontier_add (x, y) frontier
                                   else frontier
    
frontier_add (x, y) frontier = frontier ++ [(x, y)]
    
path_check m (x, y) = path_check_return (path_check_repeat [(can_move m (x, y) 'w'), (can_move m (x, y) 'd'), (can_move m (x, y) 's'), (can_move m (x, y) 'a')] 0 0)
path_check_repeat list 4 y = y
path_check_repeat list x y = if ((list !! x) == True) 
                             then path_check_repeat list (x+1) (y+1)
                             else path_check_repeat list (x+1) y
path_check_return x = if (x>1) 
                      then True 
                      else False
                      
path_straight m (x, y) frontier = path_straight_check m (x, y) [(can_move m (x, y) 'w'), (can_move m (x, y) 'd'), (can_move m (x, y) 's'), (can_move m (x, y) 'a')] frontier
path_straight_check m (x, y) list frontier = if ((((list !! 0) && (list !! 2)) || ((list !! 1) && (list !! 3))) && ((list !! 0 /= list !! 1) && (list !! 2 /= list !! 3)))
                                               then frontier
                                               else frontier_check m (x, y) frontier

list_delete_repeat list x = if (((length list)-1) /= x) 
                            then list_delete_repeat (list_delete (elem_Indices (list !! x) list) list) (x+1)
                            else list
                                        
elem_Indices (x, y) list = elemIndices (x, y) list
list_delete list list2 = (take ((head list)+1) list2) ++ (drop ((last list)+1) list2)
                                               
-- Question 9
main :: IO ()
main = do
       args <- getArgs
       solve (args)

solve :: [String] -> IO()
solve file = do 
            m <- get_maze (file !! 0)
            let x = get_path (m) (1,1) (59,19)
                y = add_dots_repeat x m 0
            print_maze (y)

add_dots_repeat list m n = if ((length list) == n)
                           then m
                           else add_dots_repeat list (place_dot m (list !! n)) (n+1)

place_dot m (x, y) = set m x y '.'