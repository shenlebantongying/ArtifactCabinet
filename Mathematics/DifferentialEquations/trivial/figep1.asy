import contour;
import graph;
size(75*4);

real ep1(real x, real y){
  return y^4 + 16y + x^4 - 8x^2;
}

draw(contour(ep1,(-10,-10),(10,10),
             new real[]{ep1(-2,-2),ep1(0,-3),ep1(0,-1),ep1(0,0),ep1(0,2)}),dotted);

// particular solution that passes (0,1)
draw(contour(ep1,(-10,-10),(10,10),
             new real[]{ep1(0,1)}),red);

xaxis(-5,5,Ticks);
yaxis(-4,3,Ticks);
