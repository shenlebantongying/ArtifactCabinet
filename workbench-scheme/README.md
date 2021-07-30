# Books

+ [r6sr](http://www.r6rs.org/) The Revised6 Report on the Algorithmic Language Scheme
+ [tspl4](https://www.scheme.com/tspl4/) The Scheme Programming Language, Fourth Edition
+ [SICP](https://mitpress.mit.edu/sites/default/files/sicp/index.html) Structure and Interpretation of Computer Programs
+ [htdp 2e](https://htdp.org/2020-8-1/Book/index.html) How to Design Programs, Second Edition
+ [EoPL3](http://www.eopl3.com/) Essentials of Programming Languages, Third Edition

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

# Interesting New Scheme

Why not embrace chaotic bizarre?

+ Gerbil <https://github.com/vyzo/gerbil>
+ Chibi -> minimum C
+ Gambit

# Links

SRFI -> <https://srfi.schemers.org/>
