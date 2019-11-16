import Data.List
import System.IO.Unsafe

---- Data Paths
-- Remember to use double backslashes for windows paths

data_path :: String
data_path = "{{path}}\\data.csv"

sales_path :: String 
sales_path = "{{path}}\\sales.txt"

---- Part A
-- Question 1
convert_single :: [Float] -> Float -> [Float]
convert_single series exchange = map (*exchange) series

-- Question 2
convert_all :: [[Float]] -> Float -> [[Float]]
convert_all series exchange = map (map (*exchange)) series

-- Question 3
days_above :: [Float] -> Float -> Int
days_above series amount = length (filter (>=amount) series)

-- Question 4
days_between :: [Float] -> Float -> Float -> Int
days_between series lower upper = length (filter (<=upper) (filter (>=lower) series)) --

----- Part B
-- Question 5
modify_position :: Float -> Float -> Float -> Float -> Float
modify_position buy_price sell_price position price
    | price < buy_price = position + 1 
    | price > sell_price = (if (position == 0) then 0 else (position-1))
    | (price >= buy_price && price <=  sell_price) = position

-- Question 6
final_position :: Float -> Float -> [Float] -> Float
final_position buy_price sell_price series = foldl (\x y -> modify_position buy_price sell_price x y) 0 series
    
modify_position_list :: Float -> Float -> [Float] -> [Float]
modify_position_list buy_price sell_price [] = []
modify_position_list buy_price sell_price (x:xs) = [(modify_position buy_price sell_price 0 x)] ++ modify_position_list buy_price sell_price (xs)
    
-- Quesion 7
daily_position :: Float -> Float -> [Float] -> [Float]
daily_position buy_price sell_price series = scanl (\x y -> modify_position buy_price sell_price x y) 0 series

-- Question 8
daily_holding_values :: Float -> Float -> [Float] -> [Float]
daily_holding_values buy_price sell_price series = zipWith (*) (series) (daily_position buy_price sell_price series)

---- Part C
-- Question 9
sales_final_position :: [String] -> [Float]
sales_final_position sales = update_list_complete (string_to_tupil_combine (splitString (sales) 0) 0) empty_list 0

splitString list x = if ((length list) == x) then [] else [words ((list) !! x)] ++ splitString list (x+1)

string_to_tupil_combine list x = if ((length list) == x) then [] else [string_to_tupil (list !! x)] ++ string_to_tupil_combine list (x+1)
string_to_tupil list = ((if ((list !! 0) == "BUY") then True else False), (read (list !! 1) :: Float), (read (list !! 2) :: Int), (read (list !! 3) :: Int))
-- True = BUY | False = SELL

update_list_complete string_list current_list 10 = current_list
update_list_complete string_list current_list current_day = if ((current_tupil_day (string_list !! 0)) == current_day) 
                                                            then update_list_complete (drop 1 string_list)(update_list (string_list !! 0) current_list) (current_day +1)
                                                            else update_list_complete string_list current_list (current_day +1)
                                         
current_tupil_day (_,_,_,x) = x

update_list (b,x,y,_) list = if b then (take y list ++ [(list !! (if ((y-1)<0) then 0 else (y-1))) + x] ++ drop (y + 1) list) 
                           else (take y list ++ [if ((list !! y) <x) then 0.0 else (list !! y) - x] ++ drop (y + 1) list)

empty_list = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0] 

-- Question 10
sales_holding_value :: [String] -> [[Float]] -> [Float]
sales_holding_value sales series =
    error "Not implemented"

---- Code for loading the data -- do not modify!

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep [] = []
splitOn sep list =
    let
        ne_sep = \ x -> x /= sep
        first = takeWhile ne_sep list
        second = dropWhile ne_sep list
        rest = if null second then [] else tail second
    in
        first : splitOn sep rest

get_data :: [[Float]]
get_data = 
    unsafePerformIO $
        do
            file <- readFile data_path
            let line_split = splitOn '\n' file
                remove_r = map (filter (/='\r')) line_split
                full_split = map (splitOn ',') remove_r
                converted = map (map (read :: String -> Float)) full_split
            return converted

get_series :: Int -> [Float]
get_series n = 
    if n >= 0 && n < 10
    then (transpose get_data) !! n
    else error ("There is no series " ++ show n)

get_sales :: [String]
get_sales =
    unsafePerformIO $
        do 
            file <- readFile sales_path
            return $ map (filter (/='\r')) $ splitOn '\n' file

short_data :: [[Float]]
short_data = take 10 get_data

get_short_series :: Int -> [Float]
get_short_series n = take 10 (get_series n)

