import Prettify
import PrettyJSON
-- Our current pretty printer is spartan so that it will fit within our space constraints, 
-- but there are a number of useful improvements we can make.
-- Write a function, fill, with the following type signature:
--   fill :: Int -> Doc -> Doc
-- It should add spaces to a document until it is the given number of columns wide. 
-- If it is already wider than this value, it should not add any spaces.

fill :: Int -> Doc -> Doc
fill width x | width `fits` p = fill (width - 1) (enclose ' ' ' ' x)
             | otherwise = x
                 where p = pretty width x
