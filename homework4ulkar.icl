module homework4ulkar
import StdEnv

//Task 1
/*
    You are given a list of integers. Your task is to remove the minimal and maximum element.
    The list should not be modified.

    In case, there are multiple occurances of a number, remove all of them.
    If it is impossible to remove the min and max elements, abort with "error"
*/
remfunc :: Int Int [Int] -> [Int]
remfunc x a [] = []
remfunc x a [y:ys]
| x == y || a == y = remfunc x a ys
= [y : remfunc x a ys]

remExtr :: [Int] -> [Int]
remExtr [] = abort "error"
remExtr [y] = abort "error"
remExtr [y:ys] 
| hd ys == y = abort "error"
= remfunc (minList[y:ys]) (maxList [y:ys]) [y:ys]


//Start = remExtr [1, 5, 2, 6, 1, 52, 1] // [5,2,6]
//Start = remExtr [6, 1, 6, 6, 3, 7, 2, 8, 3, 6, 2, 7] // [6,6,6,3,7,2,3,6,2,7]
//Start = remExtr [1, 5, 2, 6, 2, 6, 2] // [5,2,2,2]
//Start = remExtr [1, 5, 53, 25, 12, 6, 2] // [5,25,12,6,2]
//Start = remExtr [1, 15, 63, 23, 64, 23, 64, 75, 34, 63, 26, 73, 28] // [15,63,23,64,23,64,34,63,26,73,28]
//Start = remExtr [1, 2, 3, 4, 5] // [2,3,4]
//Start = remExtr [1] // error
//Start = remExtr [5, 5, 5, 5] // error

//Task 2
/*
    A train was transported across the sea by a ship, but every car was placed in a different container.
    Luckily, every car was labeled with a number (index). Help the engeneers to put the cars back in the
    right order.

    You are given a list of tuples containing a char - the name of the car, and a number - the index.
    Your task is to arrange the cars in ascending index order (first car should have the smallest index,
    and the last car will have the greatest index).

    Example: arrangeCars [('b', 1),('x', 3),('c', 2),('a', 0)] -> ['a','b','c','x']
                                                                    ^   ^   ^   ^
                                                                    0   1   2   3   (sorted)

    During the transportation, something might have gone wrong, so you are not guaranteed that the
    indexes are consecutive or unique. If they are not consecutive, just arrange them increasingly.
    If some index is repeating, leave the 
    
    Example: arrangeCars [('b', 2),('x', 0),('c', 3),('a', 0)] -> ['x','a','b','c']
                                                                    ^   ^   ^   ^
                                                                    0   0   2   3   (sorted)

    Trick: use HOF to deal with the tuples
           
           // (the map will pass every tuple to "function")
           Start = map (function) [('a', 1), ('b', 2), ('c', 3)] // ['a','b','c']

           // the function will recieve a tuple, and will return the first element of it
           function :: (Char, Int) -> Char
           function (x, y) = x 

           Also the function "sortBy" could be useful in this exercise.
           From the definition:
                sortBy  :: (a a -> Bool) [a] -> [a] 					// Sort the list, arg1 is < function
                                ^
                                |
                            a function that takes 2 parameters and compares them in order to be able to sort the list
*/

//countOcc :: Int [(Char,Int)] -> Int 
//countOcc a [(x,y):xs] = f a [(x,y):xs] 0
//	where 
//		f a [] i = i
//		f a [(x,y):xs] i 
//		| a == y = f a xs i+1
//			= f a xs i

arrangeCars :: [(Char, Int)] -> [Char]
arrangeCars [] = []
arrangeCars [(x,y):xs] = map (fst) (sortBy(\(x,y) (a,b) -> y < b) [(x,y):xs])

//Start = arrangeCars [('b', 1),('x', 3),('c', 2),('a', 0)] // ['a','b','c','x']
//Start = arrangeCars [('b', 2),('x', 0),('c', 3),('a', 0)] // ['x','a','b','c']
//Start = arrangeCars [('R', 4),('T', 2),('S', 10),('N', 9),('I', 7),('A', 5)] // ['T','R','A','I','N','S']
//Start = arrangeCars [('n', 7),('f', 5),('i', 40),('o', 42),('u', 5),('c', 12),('n', 42),('l', 99),('t', 33),('a', 42)] // ['f','u','n','c','t','i','o','n','a','l']
//Start = arrangeCars [('e', 42),('r', 121),('h', 0),('m', 9),('o', 7),('k', 121),('w', 99),('o', 99)] // ['h','o','m','e','w','o','r','k']
//Start = arrangeCars [('m', 0),('i', 0),('r', 1),('m', 1),('d', 0),('t', 0),('e', 0)] // ['m','i','d','t','e','r','m']
//Start = arrangeCars [('m', 0),('i', 0),('d', 0),('t', 0),('r', 1),('m', 1),('e', 0)] // ['m','i','d','t','e','r','m']