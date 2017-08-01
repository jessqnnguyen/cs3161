module Scratch where
  
data Foo = F | G deriving (Show, Eq)

x :: Foo
x = F
 
-- interesting cases of brackets
-- this returns c!
foo :: ((a, b) -> c) -> (a -> b -> c)
foo f a b =  f (a, b)

-- does the same thing but on a pair
-- takes two inputs again, a function (a -> -> b -> c) and a pair (a, b)
-- fum takes a function f that I can partially  apply and then applies it to second arg
fum :: (a -> b -> c) -> ((a, b) -> c)
fum f (a, b) = f a b

-- note the subtle difference in function call!
