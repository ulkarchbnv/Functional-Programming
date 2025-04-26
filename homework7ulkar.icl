module homework7ulkar
import StdEnv


//Task 1
/*
Find if a tree is balanced. To find it, we take the first node
and analyse the depth of it's sub-trees. If the left one is greater, 
then we return -1, if the right one is greater, we return 1. If they
are equal, we return 0.

Example: balance (Node (Node Leaf Leaf) Leaf) = -1
						
						N
					   / \
					  N   L
					 / \
					L   L

		 balance (Node Leaf (Node Leaf Leaf)) = 1
						
						N
					   / \
					  L   N
					     / \
					    L   L
					
		 balance (Node (Node Leaf Leaf) (Node Leaf Leaf)) = 0
						
						 N
					   /   \
					  N     N
					 / \   / \
					L   L L   L
*/

:: Tree = Node Tree Tree | Leaf

depth :: (Tree) -> Int
depth Leaf = 0
depth (Node le ri) = (max (depth le) (depth ri)) + 1

//Start = depth (Node (Node Leaf Leaf) Leaf)

balance :: Tree -> Int
balance (Node le ri)
| depth le > depth ri = -1
| depth le < depth ri = 1
| depth le == depth ri = 0

//Start = balance (Node (Node Leaf Leaf) Leaf) // -1
//Start = balance (Node Leaf (Node Leaf Leaf)) // 1
//Start = balance (Node (Node Leaf Leaf) (Node Leaf Leaf)) // 0
//Start = balance (Node (Node (Node Leaf (Node Leaf Leaf)) Leaf) (Node Leaf (Node Leaf Leaf))) // -1
//Start = balance (Node (Node Leaf (Node Leaf Leaf)) (Node Leaf (Node Leaf (Node Leaf (Node Leaf Leaf))))) // 1
//Start = balance (Node (Node Leaf (Node Leaf (Node Leaf Leaf))) (Node Leaf (Node Leaf (Node Leaf Leaf)))) // 0



//Task 2
/*
We define a type Rect for rectangles that are parallel to the axes in a
Cartesian coordinate system. We represent a rectangle by its lower left
endpoint using the Point type.

Find the sum of the areas of all the rectangles, whose lower left endpoint values
are nonnegative (>= 0).
*/

:: Point = {
		x :: Int,
		y :: Int
		}
		
:: Rect = {
		pos :: Point,
		width :: Int, 
		height :: Int
		}
		
r1 = { pos = {x = -5, y = 0}, width = 10, height = 10 }
r2 = { pos = {x = 0, y = -3}, width = 5, height = 10 }
r3 = { pos = {x = 0, y = 1}, width = 25, height = 25 }
r4 = { pos = {x = 10, y = 12}, width = 1, height = 1 }
r5 = { pos = {x = 12, y = 11}, width = 3, height = 24 }
r6 = { pos = {x = -32, y = 4}, width = 4, height = 24 }
r7 = { pos = {x = 0, y = 0}, width = 5, height = 12 }
r8 = { pos = {x = 4, y = -3}, width = 22, height = 21 }
r9 = { pos = {x = -100, y = -4}, width = 23, height = 22 }

nonegative :: Rect -> Bool
nonegative r 
| r.pos.x < 0 && r.pos.y == 0 = False
| r.pos.x == 0 && r.pos.y < 0 = False
| r.pos.x < 0 && r.pos.y > 0 = False
| r.pos.x > 0 && r.pos.y < 0 = False
| r.pos.x < 0 && r.pos.y < 0 = False
= True

sum_areas :: [Rect] -> Int
sum_areas r = sum(map (\x = x.width * x.height) (filter (\x = nonegative x) r))
//Start = sum_areas [r1, r2, r3] // 625
//Start = sum_areas [r1, r2, r3, r4, r5, r6, r7, r8, r9] // 758
// own test case
//Start = sum_areas [r1, r2, r3, r4, r5, r6] //698