#lang rhombus
import: rhombus/meta open

meta:
  syntax_class Arrows
  | '$a -> $b'
  | '$a --> $b'
  | '$a --|> $b'

expr.macro 'hello $(exp :: Arrows)!':
  'block:
     print($exp.a)
     print(" is ")
     print($exp.b)
     println()'

hello "x" -> "y"!
hello "q" --|> "w"!
