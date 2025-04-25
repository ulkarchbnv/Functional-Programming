module homework2ulkar
import StdEnv

//Task 1
/*
Sum all the numbers below (and including) x that are multipliers of 3 or 10.
You can consider that the number is positive.
*/

check :: Int -> Bool
check x
| (x > 0) && (x rem 3 == 0 || x rem 10 == 0) = True
= False
sumOfDiv :: Int -> Int
sumOfDiv x 
| x < 0 = abort "can not be negative"
| x == 0 = 0
| check x == True = x + sumOfDiv(x-1)
= sumOfDiv(x-1)

//Start = sumOfDiv 10 // 3 + 6 + 9 + 10 = 28
//Start = sumOfDiv 15 // 55
//Start = sumOfDiv 50 // 528
//Start = sumOfDiv 0 // 0

//Task 2
/*
Find the n-th index of the Jacobsthal number sequence.
a(n) = a(a - 1) + 2a(a - 2) for n >= 2, with a(0) = 0, a(1) = 1. 

Read more here: https://en.wikipedia.org/wiki/Jacobsthal_number

Check if n is valid.
*/

jacobsthal :: Int -> Int
jacobsthal n
| n == 0 = 0
| n == 1 = 1
| n >= 2 = ( 2 ^ n - ((-1) ^ n))/3 //this is how to directly calculate
//| n >= 2 = (( 2 ^ (n-1) - ((-1) ^ (n-1)))/3) + 2 * ( ( 2 ^ (n-2) - ((-1) ^ (n-2)))/3) <----- according to formula given a(n) = a(a - 1) + 2a(a - 2) for n >= 2
= abort "invalid argument"

//Start = jacobsthal 4 // 5
//Start = jacobsthal 6 // 21
//Start = jacobsthal 8 // 85
//Start = jacobsthal 16 // 21845
//Start = jacobsthal 0 // 0
//Start = jacobsthal 1 // 1
//Start = jacobsthal -4 // "invalid argument"