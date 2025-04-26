module homework8ulkar
import StdEnv


//Task
/*
Define the (<) operator for Binary Search Trees. For the sake of the exercise, we say that
a BSTree is smaller than another if it's minimum value is smaller than the other's minimum value.

Example: (BSNode 3 (BSNode 2 BSLeaf BSLeaf) (BSNode 4 BSLeaf BSLeaf)) < (BSNode 1 BSLeaf BSLeaf) == False

								 3 						1
							   /   \                   / \
							  2     4				  L   L
							 / \   / \
							L   L L   L
							
							  min == 2				 min == 1
							
									  2 < 1 // False
									  
          (BSNode 3 (BSNode 0 BSLeaf BSLeaf) (BSNode 4 BSLeaf BSLeaf)) < (BSNode 1 BSLeaf BSLeaf) == True

								 3 						1
							   /   \                   / \
							  0     4				  L   L
							 / \   / \
							L   L L   L
							
							  min == 0				 min == 1
							
									  0 < 1 // True

You can assume that there are no trees that consist only from a leaf.
*/

:: BSTree a = BSNode a (BSTree a) (BSTree a) | BSLeaf
inorder :: (BSTree Int) -> [Int]
inorder BSLeaf = [] 
inorder (BSNode a le ri) = (inorder ri) ++ [a] ++ (inorder le)
instance < (BSTree Int) 
where 
	(<) a b = minList(inorder a) < minList(inorder b)
		
//Start = (BSNode 3 (BSNode 2 BSLeaf BSLeaf) (BSNode 4 BSLeaf BSLeaf)) < (BSNode 1 BSLeaf BSLeaf) // False
//Start = (BSNode 3 (BSNode 0 BSLeaf BSLeaf) (BSNode 4 BSLeaf BSLeaf)) < (BSNode 1 BSLeaf BSLeaf) // True
//Start = (BSNode 2 (BSNode 1 BSLeaf BSLeaf) (BSNode 3 BSLeaf BSLeaf)) < (BSNode 2 (BSNode 1 BSLeaf BSLeaf) (BSNode 3 BSLeaf BSLeaf)) // False
// Start = (BSNode 2 (BSNode 0 BSLeaf BSLeaf) (BSNode 3 BSLeaf BSLeaf)) < (BSNode 2 (BSNode 1 BSLeaf BSLeaf) (BSNode 3 BSLeaf BSLeaf)) // True
// Start = (BSNode 2 (BSNode 0 BSLeaf BSLeaf) (BSNode 3 BSLeaf BSLeaf)) > (BSNode 2 (BSNode 1 BSLeaf BSLeaf) (BSNode 3 BSLeaf BSLeaf)) // True