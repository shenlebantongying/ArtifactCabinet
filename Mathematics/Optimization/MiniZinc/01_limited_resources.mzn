% same as julia's 01_limited_resources

float: a = 100;
float: b = 200;
float: c = 300;

var float: x;
var float: y;
var float: z;

constraint x>=0;
constraint y>=0;
constraint z>=0;

constraint 1*x+2*y+3*z <= a;
constraint 2*x+1*y+3*z <= b;
constraint 3*x+2*y+1*z <= c;

solve maximize x+2*y+3*z;

output [
  "hi\n",
  "x -> \(x)\n",
  "y -> \(y)\n",
  "z -> \(z)\n",
  "x+2*y+3*z -> \(x+2*y+3*z)"
]
