#lang rhombus


interface Shape:
  method area()
  method double_area():
    area()*2

class Circle(r):
  implements:
    Shape
  override area():
    3.15*r**2

class Square(x,y):
  implements:
    Shape
  override area():
    x*y

check Circle(2).double_area() ~is 25.2
check Square(2,3).double_area() ~is 12

// :)
class UnusualShape(q,w,e):
  method area():
    q*w*e

fun UnusualAreaCalc(s :: Shape || UnusualShape):
  s.area()

check UnusualAreaCalc(UnusualShape(1,2,3)) ~is 6
check UnusualAreaCalc(Circle(2)) ~is 12.6
