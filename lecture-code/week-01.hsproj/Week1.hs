module Main where

-- Example of a function that takes 2 arguments
--f :: Int -> Int -> Int
--f x y = x + y

-- HS enum
data Token = OpenP | CloseP | Plus | Minus 
  deriving (Show)

-- [Token] is a list not an array of objects of type 'Token'

lexer :: String -> [Token]
lexer [] = []
lexer ('(' : restChars) = 
  OpenP : lexer restChars
lexer (')' : restChars) = CloseP : lexer restChars
lexer (firstChar : restChars) = 
  lexer restChars
-- alternatively call firstChar '_' which is an anonmyous variable
-- lexer (_ : restChars) = lexer restChars


main = 
  putStrLn "Not yet implemented!"