
# Week 4 Lecture

Recall: There was no lecture last week!

## TODO

* [ ] Complete the sample midsession exam before Week 5

## Tuesday Lecture: Semantics

### Substitution

We write:

$$ x \ [x := t'] $$

to denote a $$$ t $$$ where all free occurences of $$$ x $$$ is replaced by $$$ t' $$$.

* Do not sub with expressions that have already bound variables in them

#### Examples

$$ x[x:=y] = y $$
$$ z[x:=y] = z, if z \ne x $$
$$ (Op t_1 .. t_n) [x := y] = Op (t_1 [x := y]) .. Op (t_2 [x := y]) $$

### Untyped $$$ \lambda $$$- Calculus

* Very simple, Turing-complete formalism for computations introduced by Alonzo Church in 1936
* Formalise the notion of computability mathematically
* Turing complete means any kind of computation in the language can be computed by the Turing machine

#### $$$ \lambda $$$ calculus rules

Three main rules:

* $$$ \alpha $$$-conversion
	* if $$$ t \equiv_{a} s $$$ then two terms are equivalent in the calculus
* $$$ \beta $$$-reduction
	* $$$ (\lambda x. t)s$$$ can be reduced to $$$t[x:=s]$$$
* $$$ \eta $$$-conversion
	* $$$ \lambda x \ (f \ x) $$$ is equivalent to $$$ f $$$ if $$$ x $$$ is not free in $$$ f $$$


#### Example of $$$ \beta $$$-reduction

![](beta-reduction-01.png)

#### Defining true and false in $$$ \lambda $$$ calculus

![](lambda-calculus-boolean-definition.png)

#### $$$ \lambda $$$ calculus in Haskell examples

* Another name for $$$ \lambda $$$ calculus is a function that doesn't have a name

```
(\ x -> x) 10

(\ x -> \y -> 2* x + y) 10 2
```

### Static semantics

* Properties of a program that are obvious without executing the program
* Can be checked by a compiler or an external tool such as `lint`

#### Scoping

* We use inference rules to check scoping

* Key idea: use an environment to keep track of all bound vars, for now this is just a set of var names

$$ {x_1, x_2, ..., x_n} ⊢ e \ ok $$

assumes vars $$$ x_1 .. x_n $$$ are bound then $$$ e \ ok $$$ holds

#### Dynamic semantics

* Specifies the program execution process
* May include side effects and computed values

##### Denotational Semantics
* Idea: syntactic expressions are mapped to mathematical objects

###### Examples

* Mapping to lambda-calculus
* fix-point semantics over complete partial orders

##### Axiomatic Semantics

* Statements over programs in the form of axioms describing logic program properties

###### Hoare's calculus

{$$$P$$$} $$$prgrm$$$ {$$$Q$$$}

###### Dijkstra's Weakest Precondition (WP) calculus

$$ wp(prgrm, Q) = P $$

* Hoare's and Dijkstra'a WP is used typically  to prove properties of C-like/imperative languages

##### Operational Semantics

* Idea: defines semantics in terms of an abstract machine

Two main forms:

* Small step semantics \ structural operational semantics (SOS): step by step execution of a program
* Big step, natural \evaluation semantics: specifies result of execution of complete programs

We look at both, small step as well as big step semantics

#### Structural Operational Semantics

##### Transition systems

A transition system which specifies the step by step eval of a program which consists of:

* a set of states S on an abstract computing device
* a set of initial states I $$$ \subseteq $$$ S
* a set of final states F $$$ F \subseteq $$$ 
* a relation $$$↦ :: S×S$$$ describe the effect of a single evaluation step on state s


#### Evaluation or Big Step Semantics

s


