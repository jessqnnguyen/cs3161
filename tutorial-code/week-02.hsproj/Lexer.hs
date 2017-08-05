module Lexer(Token, Keyword, Operator, name, lexer) where
  
-- lowercase are functions, uppercase are types

-- imported everywhere, optional
import Prelude

import Data.Char
-- ops on chars

--data Token = Kwd Keyword
--           | IntLit Int
--           | Ident Name
--           | Op Operator
--           | LParen
--           | RParen
--           | LBrace
--           | RBrace
--           | Semicolon
--           | Unrecognized  
--           deriving (Show)
           

-- difference between using 'data' and 'type'
-- data: brand new type that doesn't exist
-- type: renaming a type like typdef blah = long

-- Lists in HS!
type String = [Char]
-- data [a] = [] | a : [a] -- HS def, strings are a singly linked list, better to use Data.Text for better, faster string type
data MyList a = EmptyList | Node a (MyList a)

lexer :: String -> [Token]
lexer [] = []
lexer (' ': restStr) = lexer restStr -- get rid of leading spaces
lexer ('\n': restStr) = lexer restStr -- get rid of leading new lines
lexer ('(': restStr) = LParen : (lexer restStr) -- if char is either

-- use isSpace c to check if a char c is a space because it checks for all possible white space characters across languages

-- break function
-- takes in a predicate function, outputs a function that given a list of a's splits it into two lists of a's, 
-- break :: (a -> Bool) -> [a] -> ([a], [a])

-- function decomposition
-- (.) : (b -> c) -> (a -> b) -> (a -> c)
-- (f . g) x = f (g x)

-- total functions are functions that will give you an output for all possible inputs