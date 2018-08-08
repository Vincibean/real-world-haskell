-- Write a wrapper for traverse that lets you control traversal using one predicate, and filter results using another.

import ControlledVisit
import qualified ControlledVisit as P (traverse)

traverse' :: (Info -> Bool) -> (Info -> Bool) -> FilePath -> IO [Info]
traverse' f t = P.traverse (filter (f .&&. t))
  where (.&&.) f g a = f a && g a
