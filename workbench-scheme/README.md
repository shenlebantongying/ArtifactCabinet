# Emacs

Dont't use `scheme` layer of spacemacs or `geiser`, they are kind of overhead. Vallina scheme mode + paredit = nice.

# "Major" scheme implementations

`.scm` 
+ r5rs -> mini language, easy to implement -> r7rs-small
+ r6rs -> large language, more useful practically -> r7rs-large

+ chez -> r6rs only
+ chicken -> r5rs and use SRFI as extension, plus new r7rs and has built-in package manager to pull eggs
+ guile -> mixture of everything -> GNU-os-api, r5rs, r6rs, r7rs, SRFI plus some inspiration from lisp

Scheme community is bazaar.

`.rtk`
+ Racket

## do it in emacs

``` emacs-lisp
;; -*- geiser-scheme-implementation: guile -*-
;; -*- geiser-scheme-implementation: chicken -*-
;; -*- geiser-scheme-implementation: chez -*-
```

# Interesting New Scheme

Why not embrace chaotic bizarre?

+ MIT scheme -> the one
+ Gambit -> high performance, and actively maintained
+ Gerbil <https://github.com/vyzo/gerbil>
+ Chibi -> minimum C
+ S7 Scheme -> also minimum C
+ Gauche  https://practical-scheme.net/gauche/index.html (their doc is nice)
# Links

SRFI -> <https://srfi.schemers.org/>
Learn scheme in fixnum of days -> https://ds26gte.github.io/tyscheme/


# MISC

r6rs vs r7rs
+ SixRejections -> https://github.com/johnwcowan/r7rs-work/blob/master/SixRejection.md
+ By weinholt -> _https://weinholt.se/articles/r7rs-vs-r6rs/

# Related publications

+ Clinger, William D., and Mitchell Wand. “Hygienic Macro Technology.” Proceedings of the ACM on Programming Languages 4, no. HOPL (June 12, 2020): 1–110. https://doi.org/10.1145/3386330.

