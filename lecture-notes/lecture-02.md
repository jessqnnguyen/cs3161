# Week 2 Lecture 

## TODO

* [ ] Review Rule Induction: Example 
* [ ] Fix MathJax rendering on this file

## Tuesday Lecture

### Rule induction

* Given a set of rules $$$R = {J_1, J_2, ..., J_n}$$$ if property $$$P$$$ holds for all $$$J_i$$$ then $$$J$$$ holds.

#### Example 1

Show some property P holds for all natural numbers:

Property P(n): `n + 0 = 0 + n`

##### Show P(0) is true

Rule: `0 nat`

Hence P(0) is true.

##### Show P(n) is true

Rule : `n nat / s(n) nat`

Hence n holds for any n in nat. 

#### Example 2

Let pl(s) be the number of left parethesis and pr(s) be the number of right parenthesis.

##### Proof outline

We have to consider 3 cases, 1 case per rule.

###### Case 1: Base case

$$s = \epsilon$$

$$pl(s) = pr(s) = 0 $$

###### Case 2: Induction Hypothesis

$$pl(\ (s)\ \) =  pl( \ ( \ \) + pl( \ s ) \ \) $$

$$ = 1 + pl(\ s) \ \ ) = \ 0$$
$$ = 1 + pl(\ s \ ) + pl( \ ) \ \ )$$

#### Example 3

Define some basic arithmetic expression rules to perform operations on 2 numbers $$$e_1$$$ and $$$e_2$$$.

$$\frac{e_1 \ SExpr \ e_2 \ PExpr}{e_1 + e_2 \ \ SExp}$$

$$\frac{e_1 \ PExpr \  e_2 \ FExpr }{e_1* e_2 \ PExpr} $$


$$\frac{n \ Int}{n \ FExpr}$$

$$\frac{e \ SExpr}{(e) \ FExpr}$$

Use the rules to inductively derive the following expression:

$$ 1 + 2 * 3 \ SExpr $$

$$\frac{1 \ SExpr \ 2 * 3 \ PExpr}{1 + 2*3 \ SExpr}$$

$$\frac{2 \ PExpr \ 3 \ FExpr}{2*3 \ PExpr} $$

$$\frac{2 \ FExpr}{2 \ PExpr}$$

$$\frac{1 \ PExpr}{1 \ SExpr}$$

$$\frac{1 \ FExpr}{1 \ PExpr}$$

### Typeclasses

* What could be the tye of the function (==) which compares two data items for equality?
	* `(==) :: a -> a -> Bool`
	* `(==) :: Eq a => a -> a -> Bool`
	* if $$$a$$$ is a member of a ** type class ** `Eq`, then (==) can compare two values of this type for equality and not equal (/=)
	
* When we define a new data type we can include it into the class using deriving

```
data Token
	= OpenP
	| CloseP
	| Plus
	| Minus
	| IntLit Int
	deriving (Eq, Show)fa
	
```


#### Typeclass: Num

```
class Num a where
	(+) :: a-> a -> a
	(-) :: a -> a -> a
	(*) :: a -> a -> a
	negate :: a -> a
	abs :: a -> a
	signum :: a -> a
	fromInteger :: Integer -> a
```

* Haskell is much more restrictive with implicit conversions such as (+) on two vars say, an int and a float.

#### Infix operators

Recall:

* `(+) argPat1 argPat2`
* `(&&) argPat1 argPat2`
* `7 div 3`
* `div 7 3`

#### Tuples

We use lists when we know the element type but not the size of the list.

We use tuples when we have a collection of values of different types with a fixed size (2):

```
(True, 4) :: (Bool, Int)

('c', 4, not) :: (Char, Int, Bool -> Bool)

-- returns the sequence which doesn't cause the bool function to return false
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile isDigit "123asd5" = "123"

-- returns remaining sequence which causes the bool function to return false
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile isDigit "123asd5" = "asd5"

```

### Partial function and Currying

* Why the strange notation for functions with multiple arguments? e.g.
	* `(&&) :: Bool -> Bool -> Bool` why not `(Bool, Bool) -> Bool`?

* This is because the function type constructor -> is right associative so 
* `a -> b -> c` is actually the same as `a -> (b -> c)`

##### Example code

```
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

```

##### Playground code

```
x

x == x 

(&&) True False

--f = (*) 10

--f 90

map ((*) 10) [1, 2, 3, 4, 5]

((+) 100) 45

fum (+) (9, 8)

(foo (fum (+))) 9 8 

-- give it a function and it'll spit back a function that can be partially applied
curry 
-- the opposite!
uncurry

-- (:) 1 takes a list and puts 1 in front of it and map maps this function to args
map ((:) 1) [[1,2, 3], [3, 4]]
```
## Friday Lecture

### Judgments
* A judgment states that a certain property holds for a specific object which corresponds to a set membership
* Generally judgements express a relationship between a number of objects (n-ary relations)

#### Examples

###### Binary
* 4 divides 16 
* ail is a substring of mail

###### n-ary relation 
* Defines sets of n-tuples

* substring: {("", "mail"), ("m", "mail")}
* plus_equal: {(0,0,0), (0,1,1)}


### Relations

Recall a binary relation R is:

* symmetric 
* reflexive
* transitive

`A relation which is symmetric, reflexive and transitive is called an equivalence relation.`


### Concrete Syntax

* The inference rules for SExpr we covered last week defined the concrete syntax of a simple language - including precedence and associativity
* Designed with the human user in mind

#### Problem - ambiguous grammar!
* Concrete syntax contains too much information! There are too many ways to semantically represent the same expression which is not adequate for internal representation during compile time

##### Example

Multiple ways to represent 

* $$$1 + 2 * 3$$$
* $$$1 + (2 * 3)$$$
* $$$(1) + ((2) * (3))$$$

### Concrete vs abstract

* Concrete is user friendly
* Abstract is compiler friendly

