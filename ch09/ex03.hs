-- Using id as a control function, traverse id performs a preorder traversal of a tree: it returns a parent directory before its children. Write a control function that makes traverse perform a postorder traversal, in which it returns children before their parent.

import Data.List (sortBy)
import Data.Ord (comparing)
import ControlledVisit 
import qualified ControlledVisit as P (traverse)

postOrder :: [Info] -> [Info]
postOrder = reverse

postOrder' :: [Info] -> [Info]
postOrder' = sortBy $ flip $ comparing infoSize

-- Assume that the parent directory will always be names "." and as such will be places after its content.
postOrder'' :: [Info] -> [Info]
postOrder'' = sortBy $ flip $ comparing infoPath

traversePostOrder = P.traverse postOrder

traversePostOrder' = P.traverse postOrder'

traversePostOrder'' = P.traverse postOrder''
