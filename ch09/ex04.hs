-- Take the predicates and combinators from "Gluing Predicates Together" on page 224 and make them work with our new Info type.

import System.FilePath (takeExtension)
import ControlledVisit

type InfoP a = Info -> a

type Predicate = InfoP Bool

pathP :: InfoP FilePath
pathP = infoPath

sizeP :: InfoP Integer
sizeP nfo = case (infoSize nfo) of
  Just s -> s
  Nothing -> -1

equalP, (==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP = liftP (==)
(==?) = equalP

-- f (the Infop function) is given i (Info, from InfoP c), returning an a. a `op` b returns c
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP op f b i = f i `op` b 

greaterP, lesserP, (>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
(>?) = greaterP
(<?) = lesserP

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 op fia fib i = (fia i) `op` (fib i)

andP, orP, (&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)
(&&?) = andP

constP :: a -> InfoP a
constP k _ = k

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' = liftP

liftPath :: (FilePath -> a) -> InfoP a
liftPath f = f . pathP

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072
