# Macrology

## To read from racket community

- [Mythical macros](https://soegaard.github.io/mythical-macros/)
- [Racket Macro Exercises by Alexis](https://lexi-lambda.github.io/racket-macro-exercises/index.html)
- [Fear of Macros by GregH](https://www.greghendershott.com/fear-of-macros/index.html)
- <https://school.racket-lang.org/2019/plan/index.html> =\> syntax parse

## Readings

## Hygienic macro technology

aka. Why Lisp's macro suck.

aka. Origins and innovations of Scheme's Macro.

<https://dl.acm.org/doi/10.1145/3386330>

note. There was not macro in scheme before r4rs, and no complete
description before r5rs.

## Submodules in Racket. You Want it When, Again?

aka. Why Scheme's (load is problematic for macros.

For macro across different modules

<https://www.cs.utah.edu/plt/publications/gpce13-f-color.pdf>

also. The problem described by lisp people

in 13. Macros and Name Collisions -\> Technical Issues of Separation in
Function Cells and Value Cells.

<http://www.nhplace.com/kent/Papers/Technical-Issues.html>

Q: does r7rs solved this problem?

## Syntactic abstraction in Scheme

aka. Origin of `syntax-case`

aka. Pattern-Based Macros

<https://dl.acm.org/doi/10.1007/BF01806308>

modern-description.
<https://www.scheme.com/tspl4/syntax.html#./syntax:h3>

## Fortifying macros (syntax/parse)

aka. "Simplified", or a new kind of notation for macro writing.

<https://dl.acm.org/doi/10.1145/1863543.1863577>

<https://docs.racket-lang.org/syntax/stxparse.html>

## Pattern-Based Syntax Matching

Syntactic abstraction in Scheme

aka. Origin of `syntax-case`

aka. Pattern-Based Macros

<https://dl.acm.org/doi/10.1007/BF01806308>

modern-description. <https://www.scheme.com/tspl4/syntax.html#>.

## Syntax/Parse

See paper "Fortifying Macros"

Main doc: <https://docs.racket-lang.org/syntax/stxparse.html>

Examples <https://docs.racket-lang.org/syntax-parse-example/index.html>
