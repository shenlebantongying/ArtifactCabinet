Implementation strategy: int ID as entity index, components are grouped into vectors

This assumes that every entity has most components

````
entity   -> A  B  C  D
EntityID -> 1 2 3 4
pos      -> [] [] [] []
mov      -> [] [] [] []
```

TODO: don't uses int as ID
