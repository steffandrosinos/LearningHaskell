--                         Assignment 1                         -- 
--                    Steffan Drosinos Jones                    -- 
--                        ID - {removed}                        --
--
-- Part A

-- Question 1
-- Characters to integers
char_to_int :: Num a => Char -> a
char_to_int '0' = 0     -- Returns int 0
char_to_int '1' = 1     -- Returns int 1
char_to_int '2' = 2     -- Returns int 2
char_to_int '3' = 3     -- Returns int 3
char_to_int '4' = 4     -- Returns int 4
char_to_int '5' = 5     -- Returns int 5
char_to_int '6' = 6     -- Returns int 6
char_to_int '7' = 7     -- Returns int 7
char_to_int '8' = 8     -- Returns int 8
char_to_int '9' = 9     -- Returns int 9
char_to_int a = error "Not a valid char between 0-9"    -- Invalid input

-- Question 2
-- Repeating char
repeat_char :: (Eq t, Num t) => a -> t -> [a]
repeat_char c 0 = []                                -- Base case, when n gets to 0 we want to return an empty set
repeat_char c n = c : repeat_char c (n-1)           -- Input character and binding c to c n times (n-1 until n=0)   

-- Question 3
-- Decoder
decode :: [Char] -> [Char]
decode [] = []      -- Base case for when the  list is empty
decode (x:y:xs) = repeat_char x (char_to_int y) ++ decode xs    
{- ^^^
Giving x (which should be a char) and y (which should be an int between 0-9) 
to repeat_char and then using recursion
-}


-- Part B

-- Question 4
-- Integers to characters
int_to_char :: (Eq a, Num a) => a -> Char
int_to_char 0 = '0'     -- Returns char '0'
int_to_char 1 = '1'     -- Returns char '1'
int_to_char 2 = '2'     -- Returns char '2'
int_to_char 3 = '3'     -- Returns char '3'
int_to_char 4 = '4'     -- Returns char '4'
int_to_char 5 = '5'     -- Returns char '5'
int_to_char 6 = '6'     -- Returns char '6'
int_to_char 7 = '7'     -- Returns char '7'
int_to_char 8 = '8'     -- Returns char '8'
int_to_char 9 = '9'     -- Returns char '9'
int_to_char a = error "Not a valid integer between 0-9"     --Invalid input

-- Question 5
-- Length of input character
length_char :: Num t => Char -> [Char] -> t
length_char c string = length_char_repeat c string 0    -- Passing an input char, the string and a 0 to length_char_repeat

length_char_repeat :: Num t => Char -> [Char] -> t -> t
length_char_repeat c "" y = y       -- Base case = When the list is empty we return the value of y
length_char_repeat c (x:xs) y = if (x == c)             --if statement to see if element 1 of the list is equal to the input char
                                then (length_char_repeat c xs (y+1))    --if True then call itself with c, the tail of the list and y(+1)
                                else y      -- When x ≠ y we return the value of y
                                
-- Question 6
-- Drop character function
drop_char :: Char -> [Char] -> [Char]
drop_char c "" = ""     -- When the list is empy -> return nothing ("")
drop_char c (x:xs) = if (x == c) then drop_char c xs else x:xs      -- if statement to check if the head of the string is equal to the input char
                                                                    -- if true we pass c and xs to drop_char 
                                                                    -- if false then we return the whole string

-- Question 7
-- Encoder
encode :: [Char] -> [Char]
encode "" = ""          -- Base case
encode (x:xs) = [x,(int_to_char (length_char x (x:xs)))] ++ encode (drop_char x (x:xs))
{- ^^^
We input a string, we take the head of the string and call length_char with x and the string as input.
This returns an integer therefore we call int_to_char on this output to turn it into a character
This will then bind with x and give [xA] (where A is an int 0-9)
We then call encode again after passing x and the whole string to drop_char
We will call enocde untill the inpujt list is empty
-}

-- Part C

-- Question 8
-- Integer(s) to string
int_to_string :: Integral a => a -> [Char]
int_to_string x = combine_into_string (int_to_list x)       --Passing an Integer input into combine_into_string after passing it to int_to_list

-- Turning 123 into [1,2,3]
int_to_list :: Integral a => a -> [a]
int_to_list 0 = []  -- Base case when x = 0 return an empty list ("")
int_to_list x = int_to_list (x `div` 10) ++ [x `mod` 10]

-- Turning the elements of the ^ list into characters and then binding those characters together to make a string
combine_into_string :: (Num a, Eq a) => [a] -> [Char]
combine_into_string [] = []     -- Base case
combine_into_string list = [int_to_char (head list)] ++ combine_into_string (tail list)

-- Encoder 
complex_encode :: [Char] -> [Char]
complex_encode "" = ""  -- Base case
complex_encode (x:xs) = [x] ++ (if (length_char x (x:xs) == 1) 
                                then "" 
                                else (int_to_string (length_char x (x:xs)))) 
                            ++ complex_encode (drop_char x (x:xs))

-- Question 9
-- Numbers in String to integer
string_to_int :: Num p => [Char] -> p
string_to_int s = mult_and_add s (string_to_int_count s 0)

-- Takes a integer represented as a string and returns the corresponding Integer
mult_and_add :: (Num p, Integral t) => [Char] -> t -> p
mult_and_add [] y = 0
mult_and_add (x:xs) y = (char_to_int x) * (10^(y-1)) + mult_and_add xs (y-1)

-- Counter for how many elements there are in a list (could've used length but yolo)
string_to_int_count :: Num t => [a] -> t -> t
string_to_int_count [] y = y
string_to_int_count (x:xs) y = string_to_int_count (xs) (y+1)

-- Decoder
-- Check for Int or Char True = int False = letter
intletter_check :: Char -> Bool
intletter_check '0' = True    
intletter_check '1' = True     
intletter_check '2' = True     
intletter_check '3' = True     
intletter_check '4' = True     
intletter_check '5' = True     
intletter_check '6' = True     
intletter_check '7' = True     
intletter_check '8' = True     
intletter_check '9' = True
intletter_check c = False       -- Anything other than 0-9 returns False

-- Takes a tail of a list (e.g. 123ab) and returns only the starting integers (e.g. 123) 
input_string_to_int :: [Char] -> [Char]
input_string_to_int [] = ""
input_string_to_int (x:xs) = if (intletter_check x) then ([x] ++ (input_string_to_int (xs))) else ""

-- Takes an input and a counter (0) 
input_string_int_length :: Num p => [Char] -> p -> p
input_string_int_length [] y = 1
input_string_int_length (x:xs) y = if (intletter_check x) then (y+1) + input_string_int_length xs y else y

-- Takes an String input and gets rid of the head of the string n times
drop_string :: (Eq t, Num t) => [Char] -> t -> [Char]
drop_string [] _ = ""
drop_string (x:xs) 0 = (x:xs)
drop_string (x:xs) n = drop_string xs (n-1)

-- Decoder 
complex_decode :: [Char] -> [Char]
complex_decode [] = ""
complex_decode [x] = [x]
complex_decode (x:xs) = if (intletter_check (head xs)) 
                        then repeat_char x (string_to_int (input_string_to_int xs)) 
                             ++ complex_decode (drop_charandint (x:xs)) 
                        else ([x] ++ complex_decode xs)
{- ^^^ (if you're reading this, sorry for ^ being really messy)
The decoder takes a string input and checks to see if the head of xs is an int or a letter
if true -> The function calls repeat_char with input x and passes xs to input_string_to_int through string_to_int
                                                                  ^ this is to get the String into an Integer
if false it means that the next element is a new character which means that the previous character needs to be muliplied by one
We call complex_decode until there are no elements in the list and we return "" 
-}      
                
-- Drops the first char + integer 
drop_charandint :: [Char] -> [Char]
drop_charandint (x:xs) = drop_string (x:xs) ((input_string_int_length (xs) 0)+1)
