module homework3ulkar
import StdEnv


//Task 1
/*
An element in a list is tiny if it is smaller than all elements to its
right. You will be given a list and your task will be to return a list of
all tiny elements. For example:

[1,21,4,7,5] -> [1,4,5] because 1, 4 and 5 are smaller any elment 
						to their right. 
[1,2,3,4,5] -> [1,2,3,4,5]

Notice that the last element is always included because there are no
elements smaller to it's right. All numbers will be greater than 0.
*/

tiny_nums :: [Int] -> [Int]
tiny_nums [] = []
tiny_nums [x] = [x]
tiny_nums [x:xs]
| x < minList xs = [ x : tiny_nums xs]
= tiny_nums xs
//Start = tiny_nums [1,21,4,7,5] // [1,4,5]
//Start = tiny_nums [2,5,14,3,14,17,16] // [2,3,14,16]
//Start = tiny_nums [66,78,119,99,123,88,55] // [55]
//Start = tiny_nums [11,47,42,15,13,55] // [11,13,55]
//Start = tiny_nums [67,54,27,85,66,24,51,88,49] // [24,49]
//Start = tiny_nums [76,17,25,36,29] //[17,25,29]
//Start = tiny_nums [3,18,37,9,36,47,28] // [3,9,28]

//Task 2
/*
You live in the city of Cartesia where all roads are laid out in a perfect grid. You arrived
ten minutes too early to an appointment, so you decided to take the opportunity to go for a
short walk.

The city provides its citizens with a "Walk Generating App" on their phones -- everytime you
press the button it sends you an array of one-letter strings representing directions to
walk (eg. ['n', 's', 'w', 'e'], meaning: North, South, West, East). You always walk only
a single block for each letter (direction) and you know it takes you one minute to traverse
one city block.

So create a function that will return True if the "Walk Generating App" will take you exactly
ten minutes (you don't want to be early or late!) and will, of course, return you to your
starting point. Return False otherwise.

Hint: You might want to use the cartesian coordinate system (x, y) to remember where you are
relative to your starting point (0, 0).
*/
countOcc :: Char [Char] -> Int
countOcc a [x:xs] = f a [x:xs] 0
	where 
	f a [] i = i
	f a [x:xs] i 
	| a == x = f a xs i+1
	= f a xs i
isValidWalk :: [Char] -> Bool
isValidWalk [x:xs]
| length [x:xs] == 10 && (countOcc 'n' [x:xs]) == (countOcc 's' [x:xs]) && (countOcc 'w' [x:xs]) == (countOcc 'e' [x:xs]) = True 
= False 
//Start = isValidWalk ['n','s','n','s','n','s','n','s','n','s'] // True
//Start = isValidWalk ['n','s','n','s','n','s','n','s','n','n'] // False
//Start = isValidWalk ['n','s'] // False
//Start = isValidWalk ['n','s','e','w','n','s','e','w','n','s'] // True
//Start = isValidWalk ['n','s','e','w','n','s','n','s','n','s','w','n','s','e'] // False