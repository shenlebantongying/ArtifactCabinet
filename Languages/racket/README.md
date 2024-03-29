# Emacs

Dont't use `scheme` layer of spacemacs or `geiser`, they are kind of
overhead. Vallina scheme mode + paredit = nice.

# "Major" scheme implementations

`.scm` + r5rs -\> mini language, easy to implement -\> r7rs-small + r6rs
-\> large language, more useful practically -\> r7rs-large

- chez -\> r6rs only
- chicken -\> r5rs and use SRFI as extension, plus new r7rs and has
  built-in package manager to pull eggs
- guile -\> mixture of everything -\> GNU-os-api, r5rs, r6rs, r7rs, SRFI
  plus some inspiration from lisp

Scheme community is bazaar.

`.rtk` + Racket

## do it in emacs

``` commonlisp
;; -*- geiser-scheme-implementation: guile -*-
;; -*- geiser-scheme-implementation: chicken -*-
;; -*- geiser-scheme-implementation: chez -*-
```

# Interesting New Scheme

Why not embrace chaotic bizarre?

- MIT scheme -\> the one
- Gambit -\> high performance, and actively maintained
- Gerbil <https://github.com/vyzo/gerbil>
- Chibi -\> minimum C
- S7 Scheme -\> also minimum C
- Gauche <https://practical-scheme.net/gauche/index.html> (their doc is
  nice)

Scheme that works on M1 mac: guile, gambit-scheme, chibi-scheme, chicken

It is another type of parallel execution by limiting how long a proc can
do, but how can it be implemented in other languages?

What does it do? -\> Set a time limit for a unit of operations, and let
the unit run in a separate thread. -\> If the time limit is reached, the
unit will be paused. !!-\> AND, it may be resumed with another time
limit.
