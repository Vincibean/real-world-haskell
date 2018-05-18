-- The words function counts the number of words in a string. Modify the WC.hs example in order to count the number of words in a file.
main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
