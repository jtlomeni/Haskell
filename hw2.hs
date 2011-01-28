-- CSci 555, Functional Programming, Fall 2010
-- Assignment #2

-- J.T. Lomenick
-- 7 October 2010
-- Homwork is at the End of the Thompson Definitions

-- Add an import of the Char module to allow functions like isAlphaNum to
-- work

import Char

--  BEGIN function defintions from section 7.6 of
--  S. Thompson. _Haskell: The Craft of Functional Progamming_ 
--  Second Edition, Addison Wesley, 1999.

-- Type synonyms for words and lines.
type Word = String
type Line = [Word]

-- The "whitespace" characters.
whitespace :: String
whitespace = ['\n','\t',' ']

-- Get a word from the front of a string.
getWord :: String -> String
getWord []                = [] 
getWord (x:xs) 
    | elem x whitespace   = []
    | otherwise           = x : getWord xs
    
-- Drop the first word of a string.
dropWord :: String -> String
dropWord []               = []
dropWord (x:xs) 
    | elem x whitespace   = (x:xs)
    | otherwise           = dropWord xs

-- Remove the whitespace character(s) from the front of a string.
dropSpace :: String -> String
dropSpace []              = []
dropSpace (x:xs) 
    | elem x whitespace   = dropSpace xs
    | otherwise           = (x:xs)

-- Split a string into words.
splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

-- Split into lines of length at most lineLen.
lineLen :: Int
-- lineLen = 80
lineLen = 7	-- for testing purposes

-- Get a line from a list (name changed from Thompson book),
getLine2 :: Int -> [Word] -> Line
getLine2 len []           = []
getLine2 len (w:ws)
    | length w <= len     = w : restOfLine  
    | otherwise           = []
    where
        newlen      = len - (length w + 1)
        restOfLine  = getLine2 newlen ws

-- Get a line from a list, with error repaired.
-- If a word is longer than the total line length, textbook solution for
-- splitLines did not terminate.  The repair is to make getLine keep the
-- entire word as a line by itself.

getLine3 :: Int -> [Word] -> Line
getLine3 len []      = []
getLine3 len (w:ws)
    | lenword <= len = w : getLine3 newlen ws
    | len == lineLen = [w]  -- word too long, line by itself
    | otherwise      = []
    where
        lenword = length w
        newlen  = len - (lenword + 1)


-- Split into lines. (Changed to use getLine3.)
splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = getLine3 lineLen ws
                    : splitLines (dropLine lineLen ws)

-- Fill a text string into lines.
fill :: String -> [Line]
fill = splitLines . splitWords

-- END function definitions from Thompson textbook

-----------------------------------------------------------------
---------BEGIN HOMEWORK DEFINITIONS -----------------------------
-----------------------------------------------------------------


-- 7.19 Drop the first line from a list of words.
dropLine :: Int -> [Word] -> Line
dropLine len [] = []
dropLine len (w:ws)
	| lenword  <= len =  dropLine newlen ws
	| otherwise = ws
	where
		lenword = length w
		newlen = len - (lenword + 1)


		
-- 7.20 Turn line into printable form.
joinLine :: Line -> String
joinLine [] = []
joinLine (w:ws)
	| ws == [] =  w
	| otherwise =  w ++ " " ++  joinLine ws



-- 7.21 Joins multiple lines into printable form separated
-- by newlines '\n'
joinLines :: [Line] -> String
joinLines [] = []
joinLines (w:ws)
	| ws == [] = joinLine w
	| otherwise = joinLine w ++ "\n" ++ joinLines ws



-- 7.24 Given a text string, returns the number of characters,
-- words, and lines in the string

countLines :: String -> Int
countLines [] = 1
countLines (w:ws)
	| w == '\n' =  1 + countLines ws
	| otherwise =  0 + countLines ws

countChars :: String -> Int
countChars [] = 0  
countChars (w:ws)
	|  elem w whitespace =  0 + countChars ws
	|  otherwise = 1 + countChars ws

countWords :: String -> Int
countWords [] = 0
countWords ws = 1 + countWords(dropSpace(dropWord ws))

wc :: String -> (Int, Int, Int)
wc [] = (0,0,0)
wc ws = (countChars ws, countWords ws, countLines ws)

wcFormat :: String -> (Int,Int,Int)
wcFormat [] = (0,0,0)
wcFormat ws = (countChars ws, countWords ws, countLines ws)



-- 7.25 isPalin tests whether a string is a palindrome
isPalin :: String -> Bool
isPalin [] = True
isPalin ws 
	| convertString ws == xs = True
	| otherwise = False
	where xs = convertString ( reverse ws )



ignores = [' ' , ',' , '\'', '.', '!', '?']

convertString :: String -> String
convertString [] = []
convertString (w:ws)
	|elem w ignores =  convertString ws
	|otherwise = toLower w : convertString ws

--7.26 
subst :: String -> String -> String -> String
subst oldSub newSub st
	| xs == [] = [] 
	| head xs == oldSub = joinLine (newSub  : tail xs)
	| otherwise = head xs ++ " " ++ subst oldSub newSub (joinLine (tail xs))
	where xs = splitWords st


-- 9.23 Generalizations to plymorphic higher-order functions
-- getLine, dropLine, splitLines

-------------------------------------------------------------------
----------------------------BEGIN TESTING DEFINITIONS--------------
-------------------------------------------------------------------
--testing functions




line0 = []
line1 = ["one"]
line2 = ["This", "is", "a", "test"]
line3 = ["A", "messed", "", "up", "Line"]
line4 = ["983838473", "is"]


test_lines_display =
	do	putStrLn "line0 = []"
		putStrLn "line1 = [\"one\"]"
		putStrLn "line2 = [\"This\", \"is\", \"a\", \"test\"]"
		putStrLn "line3 = [\"A\", \"messed\", \"\", \"line\"]"
		putStrLn "line4 = [\"983838473\", \"is\"]"



string0 = ""
string1 = "this is a test"
string2 = "hello everyone\n what is up"
string3 = "hi\n\n what"
string4 = "wammy"
string5 = "Madam I'm Adam"
string6 = "dad"
string7 = "Mom"

test_strings_display = 
	do 
		print string0 
		print string1 
		print string2 
		print string3 
		print string4 
		print string5 
		print string6 
		print string7
	
--Test dropLine
test_dropLine =
	do 
		putStrLn "<....testing dropLine....>"
		test_lines_display
		putStrLn " "
		print (dropLine lineLen line0)
		print (dropLine lineLen line1)
		print (dropLine lineLen line2)
		print (dropLine lineLen line3)
		print (dropLine lineLen line4)


--Test joinLine
test_joinLine =
	do 
		putStrLn "<....testing joinLine....>"
		test_lines_display
		putStrLn " "
		print(joinLine line0)
		print(joinLine line1)
		print(joinLine line2)
		print(joinLine line3)
		print(joinLine line4)
		
--Test joinLines
test_joinLines = 
	do 
		putStrLn "<....Testing joinLines....>"
		test_lines_display
		putStrLn " "
		print(joinLines [line0,line1,line2])
		print(joinLines [line2,line4])
		
--test wc and wcFormat
test_wc =
	do 
		putStrLn "<....testing wc....>"
		test_strings_display
		print(wc string0)
		print(wc string1)
		print(wc string2)
		print(wc string3)
		print(wc string4)
		print(wc string5)
		print(wc string6)
		print(wc string7)
		putStrLn " "
		putStrLn "<....testing wcFormat....>"
		print(wcFormat string0)
		print(wcFormat string1)
		print(wcFormat string2)
		print(wcFormat string3)
		print(wcFormat string4)
		print(wcFormat string5)
		print(wcFormat string6)
		print(wcFormat string7)
		
--test isPalin
test_isPalin =
	do 
		putStrLn "<....testing isPalin....>"
		test_strings_display
		print(isPalin string0)
		print(isPalin string1)
		print(isPalin string2)
		print(isPalin string3)
		print(isPalin string4)
		print(isPalin string5)
		print(isPalin string6)
		print(isPalin string7)
		
--test subst
test_subst =
	do 
		putStrLn "<....Testing subst....>"
		putStrLn "Testing subst \"much\" \"tall\" \"how much is that?\" "
		print(subst "much" "tall" "how much is that?")
		putStrLn "Testing subst \"hi\" \"howdy\" \"hi how are you\" "
		print(subst "hi" "howdy" "hi how are you")
		putStrLn "Testing subst \"big\" \"huge\" \"that is a massive elephant\" "
		print(subst "big" "huge" "that is a massive elephant")

		
--test_all
test_all = 
	do
		test_dropLine
		putStrLn " "
		test_joinLine
		putStrLn " "
		test_joinLines
		putStrLn " "
		test_wc
		putStrLn " "
		test_isPalin
		putStrLn " "
		test_subst

