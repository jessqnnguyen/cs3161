# Week 7 Lecture

## Sum-types

* Union types / composite types
* Alternatives differ only in name (enums)

`data Colour = Red | Green | Blue `

* Case distinction by pattern matching

What MinHS type corresponds to the above Haskell type?

![](week-07-1.png)

### Static Semantics: Typing rules

![](sum-type-static-typing-rules.png)

Note on Inl typing rule: 

`Inl (1 + 3) :: Int + Bool`

So all we know from Inl is that it returns the  type of the left term

#### Haskell example

```
import Prelude hiding (Either)
data Either a b = InL a | InR b

foo :: Either Int Bool
foo = InL 6
```

### Dynamic Semantics: Typing rules

![](sum-type-dynamic-typing-rules.png)
