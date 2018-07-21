-- What should you pass to traverse to traverse a directory tree in reverse alphabetical order?

import Data.List (sortBy)
import Data.Ord (comparing)
import ControlledVisit 
import qualified ControlledVisit as P (traverse)

-- for useful hints on ordering, see: https://ro-che.info/articles/2016-04-02-descending-sort-haskell
reverseAlphabetical :: [Info] -> [Info]
reverseAlphabetical = sortBy $ flip $ comparing infoPath

traverseReverseAlphabetical = P.traverse reverseAlphabetical
