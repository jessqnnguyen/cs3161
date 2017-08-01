# Week 2 Lecture 

## TODO

* [ ] Review Rule Induction: Example 2

## Rule induction

* Given a set of rules $$$R = {J_1, J_2, ..., J_n}$$$ if property $$$P$$$ holds for all $$$J_i$$$ then $$$J$$$ holds.

### Example 1

Show some property P holds for all natural numbers:

Property P(n): `n + 0 = 0 + n`

#### Show P(0) is true

Rule: `0 nat`

Hence P(0) is true.

#### Show P(n) is true

Rule : `n nat / s(n) nat`

Hence n holds for any n in nat. 

### Example 2

Let pl(s) be the number of left parethesis and pr(s) be the number of right parenthesis.

#### Proof outline

We have to consider 3 cases, 1 case per rule.

##### Case 1: Base case

$$s = \epsilon$$

$$pl(s) = pr(s) = 0 $$

##### Case 2: Induction Hypothesis

$$pl(\ (s)\ \) =  pl( \ ( \ \) + pl( \ s ) \ \) $$

$$ = 1 + pl(\ s) \ \ ) = \ 0$$
$$ = 1 + pl(\ s \ ) + pl( \ ) \ \ )$$

### Example 3

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

## Typeclasses

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


### Typeclass: Num

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