module homework5ulkar
import StdEnv

//Task 1
/*
You are given a string and a letter and you need to return the index of the
second occurrence of that letter in the string. If there is no such letter
in the string, then the function should return -1. If the letter occurs only
once in the string, then -1 should also be returned.
*/
index :: Char [Char] -> Int 
index c [x:xs]
| x == c = 0
= 1 + index c xs 

strList :: String -> [Char]
strList str = [c \\ c<-:str]
//Start = strList "hello"

countOccurences :: Char [Char] -> Int 
countOccurences c [x:xs] = f c [x:xs] 0 
where 
	f c [] i = i
	f c [x:xs] i
	| c == x = f c xs i+1
	= f c xs i

second_occ :: String Char -> Int
second_occ "" c = -1
second_occ str c 
| isMember c (strList str) == True && countOccurences c (strList str) <> 1  = 1 + index c (removeMember c (strList str))
= (-1)



//Start = second_occ "Hello world!!!" 'l' // 3
//Start = second_occ "Hello world!!!" 'o' // 7
//Start = second_occ "aasdddasssda" 'd' // 4
//Start = second_occ "xaasdddasssda" 'x' // -1
//Start = second_occ "xaasdddasssdax" 'x' // 13
//Start = second_occ "Hello world!!!" 'A' // -1
//Start = second_occ "" 'q' // -1
//Start = second_occ "Hello" '!' // -1

//Task 2
/*
	You are given a list, a number and a boolean value.
	Move all the occurances of the given number in the list to the right of
	the list, if the boolean is true, and to left if false.
	
	Example: move_number [1,2,0,3,4,0,5,6,0] 0 False -> [0,0,0,1,2,3,4,5,6]
			 all the zeroes were moved to the left
*/
//removeElem :: Int [Int] -> [Int]
//removeElem n [] = [] 
//removeElem n [x:xs]
//| x == n = removeElem n xs
//= [x:removeElem n xs]
//Start = removeElem 3 [1,2,3,3,3,4]

movetoR :: Int [Int] -> [Int] 
movetoR n [] = []
movetoR n [x] = [x] 
movetoR n [x:xs]
| x == n = movetoR n xs ++ [x]
= [x] ++ movetoR n xs
//Start = movetoR 0 [1,2,0,3,4,0,5,6,0]

movetoL :: Int [Int] -> [Int]
movetoL n [] = []
movetoL n [x] = [x] 
movetoL n [x:xs]
| x == n = [x] ++ movetoL n xs
= movetoL n xs ++ [x]
//Start = movetoL 0 [1,2,0,3,4,0,5,6,0]

move_number :: [Int] Int Bool -> [Int]
move_number [] n y = [] 
move_number [x:xs] n y
| y == True = movetoR n [x:xs]
| y == False = movetoL n [x:xs]

//Code works for the condition true. For false it works, but changes the place of all other elements.

//Start = move_number [1,2,0,3,4,0,5,6,0] 0 False // [0,0,0,1,2,3,4,5,6]
//Start = move_number [1,2,0,3,4,0,5,6,0] 0 True // [1,2,3,4,5,6,0,0,0]
//Start = move_number [12,0,10,0,8,12,7,6,0,4,10,12,0] 0 True // [12,10,8,12,7,6,4,10,12,0,0,0,0]
//Start = move_number [12,54,65,22,42,12,53,12,55,87] 12 True // [54,65,22,42,53,55,87,12,12,12]
//Start = move_number [12,54,65,22,42,12,53,12,55,87] 10 True // [12,54,65,22,42,12,53,12,55,87]
//Start = move_number [34,21,22,55,23,12] 34 False // [34,21,22,55,23,12]
//Start = move_number [34,21,22,55,23,12] 34 True // [21,22,55,23,12,34]