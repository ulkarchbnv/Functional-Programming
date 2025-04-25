module homework1ulkar
import StdEnv


/*
Task 1:

Help a student find if the three given lines can form a triangle.

If the lines don't form a triangle return "Invalid Triangle";
If the lines do form a triangle then find it's type and return it:
    "Equilateral" -> all the sides are of equal length
    "Isosceles" -> only two of the sides are equal
    "Scalene" -> no sides are of the same length

You can assume that the given lines have positive lenth.
*/
//using triangle inequality theorem 
checktriangle :: Int Int Int -> Bool
checktriangle a b c
| a + b > c && a + c > b && b + c > a  = True
= False
//Start = checktriangle 3 3 3
//Start = checktriangle 100 5 1
comparison :: Int Int Int -> String
comparison a b c 
| checktriangle a b c == True && a == b && b == c = abort " Equilateral"
| checktriangle a b c == True && a == b || a == c || b == c = abort "Isosceles"
| checktriangle a b c == True && a <> b && a <> c && b <> c = abort "Scalene"

determine_triangle:: Int Int Int -> String
determine_triangle a b c
| checktriangle a b c == True = comparison a b c
= abort "Invalid Triangle"

//Start = determine_triangle 3 3 3 // "Equilateral"
//Start = determine_triangle 16 12 13 // "Scalene"
//Start = determine_triangle 20 5 20 // "Isosceles"
//Start = determine_triangle 1000 1000 5 // "Isosceles"
//Start = determine_triangle 130 150 140 // "Scalene"
//Start = determine_triangle 3 3 100 // "Invalid Triangle"
//Start = determine_triangle 100 5 1 // "Invalid Triangle"
//Start = determine_triangle 5 100 2 // "Invalid Triangle"

/*
Task 2:

To ensure the integral structure of a bridge, engineers use beams for support.

A bridge has a certain number of beams of the same width and the
distances between the beams are the same.

You are given the number of beams, their width and the distance 
between them. Calculate the length of the bridge.

Be careful to check for wrong input (the inputs cannot be negative or zero)
If the input is wrong, abort the funtion with the text "wrong input"
*/

bridge_length :: Int Int Int -> Int
bridge_length x y z
| x <= 0 || y <= 0 || z <= 0 = abort "Wrong input"
= x * y + z * (x-1)

//Start = bridge_length 2 10 20 // 40
//Start = bridge_length 5 15 25 // 175
//Start = bridge_length 3 13 24 // 87
//Start = bridge_length 5 0 25 // "wrong input"
//Start = bridge_length -1 15 25 // "wrong input"