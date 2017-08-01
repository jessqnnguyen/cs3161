# Week 1 Lecture

* No lecture week 3
* Mid session exam week 5!
* 2nd hour on Friday lecture is optional, for revision

## TODO

- [  ] Complete the assumed knowledge exercise


## Assessment

* Tutorial Participation 10%
* Mid-sem exam 10%
* Assignments 30%
* Exam 50%

Assignments released in Week 5 and Week 9.

## Assumed knowledge

* Boolean logic
* Predicate logic
* Induction over natural numbers
* No tute this week but there are exercises for Week 1 this week which will give you an idea of gaps 

> How do 3161 topics relate to the implementation of programming languages?

How does source language transform into machine code?

* Compiler
* Interpreter
 
#### Compiler vs Interpreter
* Interpreting is much slower, limited expression
* An interpeter is a virtual machine which sits on top of the concrete machine and executes source language operations
* Interpreters - easier to experiment with, faster translation, less clunky 

#### Stages of a compiler

```
int foo() {
	int i;
	i = 11;
	if (i == 11) {
		// Do something!
	}
}
```

1. First it reads in the string and decomposes the program string into a sequence of tokens (i.e. programming language objects)

```
"int foo() {\n i = 11;\n if ( .."
``` 
2. Removes trailing whitespaces, new lines, comments
3. Checks if any illegal 'words' are detected
4. Parser - the **lexer** creates a flat sequence of tokens then the **parser** checks if the token sequence adheres to the grammar and groups the tokens into a hierarchical parse tree

##### Summary

| Input | Stage | Output |
| ----- | ------| -------|
| Program string | Lexer | Sequence of tokens |
| Sequence of tokens | Parser  | Parse tree |
| Parse Tree | Semantic Analysis | Annotated parse tree |
| Annotated parse tree | Optimiser  | Intermediate code |
| Intermediate code --> Annotated parse tree | Code generation  | Machine code |


### In this course

We are looking at:

* specify the grammar of programming languages
* specify and analyse sttaic and dynamic properties of different languages and language features
* discuss and prove the correctness of different optimisations



