{--
COMP304 Assignment 1: Basic Haskell
@author: Karu Skipper

1) Data Compression

I have set this string to use in command line to make it easier for running & testing.
--}

string = "Hmmmmmh, soooooo good!!!!"

{--
Convert function:
Grabs the head of the list, then recursively calls the function.
Sorting it into a list of tuples, each with a value of 1.
I have initially set the Integer to 1, as this will be altered and used in the compact function.
--}

convert :: String -> [(Integer,Char)]
convert [] = [] 
convert (x:xs) = (1,x):(convert xs)
{--
Testing Convert function:
All these should return true if encoding function is working correctly
    Run testConvert in cmd to test
--}
t1Convert = convert [] == []
t2Convert = convert "Testing" == [(1,'T'),(1,'e'),(1,'s'),(1,'t'),(1,'i'),(1,'n'),(1,'g')]
t3Convert = convert "fflff" == [(1,'f'),(1,'f'),(1,'l'),(1,'f'),(1,'f')]
t4Convert = convert "123321" == [(1,'1'),(1,'2'),(1,'3'),(1,'3'),(1,'2'),(1,'1')]

testConvert = [t1Convert,t2Convert,t3Convert,t4Convert]
{--
Compact function:
Takes the altered list and eliminates duplicates and sets the count of the number of duplicates

-- i refers to the count of the letter

-- base case compact i [xs] = [(i, snd (xs))] checks the end of the list and puts the current 
   i value before the end of the list and places the last character there.
   compact i [] = [] checks for empty list
   then we move onto the actual recursive calls of the compact function

-- first I used the if statement to identify the character of the first tuple matches the second
    if it doesnt then print the tuple with the current i value then recursively
   call the method with the rest of the list and the count reset at 1 and add them together

-- otherwise if its matching then increase the count and call the rest of the list with the updated count
    to see if theres another another match next to it

--}

compact :: Integer -> [(Integer, Char)] -> [(Integer, Char)]
compact i [] = []
compact i [xs] = [(i, snd (xs))]
compact i (x:xs)
               | snd (x) /= snd (head xs) = (i, snd (x)):(compact 1 xs)
               | otherwise = compact (i + 1) xs

{--
Testing Compact function:
All these should return true if encoding function is working correctly
    Run testCompact in cmd to test
--}
t1Compact = compact 1 [] == []
t2Compact = compact 1 [(1,'1'),(1,'2'),(1,'3'),(1,'3'),(1,'2'),(1,'1')] == [(1,'1'),(1,'2'),(2,'3'),(1,'2'),(1,'1')]
t3Compact = compact 1 [(1,'o'),(1,'o'),(1,'o'),(1,'o'),(1,'o'),(1,'l'),(1,'o'),(1,'o'),(1,'o')]  == [(5,'o'),(1,'l'),(3,'o')]
t4Compact = compact 1 [(1,'1'),(1,'2'),(1,'3'),(1,'3'),(1,'2'),(1,'1')] == [(1,'1'),(1,'2'),(2,'3'),(1,'2'),(1,'1')]
         
testCompact = [t1Compact,t2Compact,t3Compact,t4Compact]

{--
Encode function:
Uses the convert function to convert the string into the required list of tuples
Then the compact function acts on the return list to fine tune the list to remove duplicates, parsing in the starting count of 1
This then returns the rle list of tuples
--}

encode :: String -> [(Integer, Char)]
encode x = compact 1 (convert x)

{--
Testing Encode function:
All these should return true if encoding function is working correctly
    Run testEncode in cmd to test
--}

t1Encode = encode [] == []
t2Encode = encode "Testing" == [(1,'T'),(1,'e'),(1,'s'),(1,'t'),(1,'i'),(1,'n'),(1,'g')]
t3Encode = encode "ooooolooo" == [(5,'o'),(1,'l'),(3,'o')]
t4Encode = encode "123321" == [(1,'1'),(1,'2'),(2,'3'),(1,'2'),(1,'1')]
t5Encode = encode string == [(1,'H'),(5,'m'),(1,'h'),(1,','),(1,' '),(1,'s'),(6,'o'),(1,' '),(1,'g'),(2,'o'),(1,'d'),(4,'!')]

testEncode = [t1Encode,t2Encode,t3Encode,t4Encode,t5Encode]

{--
Setup a the list so we can run in command line and test later
--}
list = [(1,'H'),(5,'m'),(1,'h'),(1,','),(1,' '),(1,'s'),(6,'o'),(1,' '),(1,'g'),(2,'o'),(1,'d'),(4,'!')]

{--
Decode function:
First we will parse in the list that needs to be decoded
Then we will use the times method to compare values if the count of the char is > 0 and concat the xs on the end
If the result is >= 0 then we have a guard for exhuastive patterns that just returns an empty string
In the times function call we retrieve the returned string and concat 

--}

decode :: [(Integer, Char)] -> String
decode [] = ""
decode (x:xs) 
            | fst(x) > 0 = times (fst(x)) (snd(x))++decode(xs)
            | otherwise = ""
{--
Times function:
This parses 2 paramters, 1 as an integer, 1 as a char
The goal is to de-increment the count and print the string as the function recusive calls
Here we also have a guard for exhuastive patterns that returns an empty string if >= 0
Here we concat the y value as we recurse through the function, returning a string
--}
times :: Integer -> Char -> String
times -1 = -1
times x y 
        | x > 0 = times ((subtract 1) x) y ++[y]
        | otherwise = ""

{--
Testing Decode function:
All these should return true if decoding function is working correctly
    Run testDecode in cmd to test
--}
t1Decode = decode [] == []
t2Decode = decode [(1,'T'),(1,'e'),(1,'s'),(1,'t'),(1,'i'),(1,'n'),(1,'g')] == "Testing"
t3Decode = decode [(5,'o'),(1,'l'),(3,'o')] =="ooooolooo"
t4Decode = decode [(1,'1'),(1,'2'),(2,'3'),(1,'2'),(1,'1')] == "123321"
t5Decode = decode [(1,'H'),(5,'m'),(1,'h'),(1,','),(1,' '),(1,'s'),(6,'o'),(1,' '),(1,'g'),(2,'o'),(1,'d'),(4,'!')] == string 

testDecode = [t1Decode,t2Decode,t3Decode,t4Decode,t5Decode]
