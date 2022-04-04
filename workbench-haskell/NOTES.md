# Polymorphism

Haskell has excellent support for polymorphic functions
+ Haskell supports parametric polymorphism, where a value may be of any type
+ Haskell also supports ad hoc polymorphism, where a value may be one of a set of types that support a particular group of operations

Haskell’s ad hoc polymorphism is provided by Type Classes, which specify a
group of operations that can be performed on a type (think Java Interfaces).

```
Prelude> :t (==)
(==) :: Eq a => a -> a -> Bool
```