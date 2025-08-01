* “Each-of”: A compound type t describes values that contain each of values of type t1, t2, ..., and
tn.

* “One-of”: A compound type t describes values that contain a value of one of the types t1, t2, ..., or tn.

```text
datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza
```

mytype can only be "one of" the 3 types

=> as type is not certain, you cannot access it directly

=> thus u need pattern matching to check its type then process the values accordingly
