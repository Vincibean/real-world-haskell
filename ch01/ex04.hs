-- Modify the WC.hs example again, in order to print the number of characters in a file.
main = interact wordCount
    where wordCount input = show (length input) ++ "\n"
