# Prolog

## Summary

+ Prolog is a language that is useful for doing symbolic and logic-based computation.
+ It‟s declarative: very different from imperative style programming like Java, C++, Python,…
+ A program is partly like a database but much more powerful since we can also have general
rules to infer new facts!
+ A Prolog interpreter can follow these facts/rules and answer queries by sophisticated search


## Quick links

Learn Prolog Now! <http://www.let.rug.nl/bos/lpn//lpnpage.php?pageid=online>
prolog :- tutorial <https://www.cpp.edu/~jrfisher/www/prolog_tutorial/contents.html>


adventure in prolog <https://www.amzi.com/AdventureInProlog/advtop.php>

Good summary <http://www.cs.toronto.edu/~hojjat/384w10/>

Common "keywords" with example <http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/>

## Difference between `fact` and `rule`:

A rule is a general statement about objects and their relationships.

A rule is all facts that follow same pattern.

Instead of 

```prolog
likes(wallace, gromit).
likes(wallace, tom).
....
```
one can imply wallace like everyone by
```prolog
likes(wallace,X) :- true.
```
Or `Wallace is friend to anyone who love cheese`

```prolog
friend(wallace,X) :- likes(X,cheese).
```

# TODO
+ prolog box model
+ list representation

## Bibliography

By the main developer of SWI-prolog:

Wielemaker, Jan, Tom Schrijvers, Markus Triska, and Torbjörn Lager. "Swi-prolog." Theory and Practice of Logic Programming 12, no. 1-2 (2012): 67-96.
