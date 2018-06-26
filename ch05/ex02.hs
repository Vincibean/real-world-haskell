-- Our pretty printer does not take nesting into account. Whenever we open parentheses, braces, or brackets, 
-- any lines that follow should be indented so that they are aligned with the opening character until a 
-- matching closing character is encountered.
-- Add support for nesting, with a controllable amount of indentation:

-- TODO
fill :: Int -> Doc -> Doc
fill = undefined
