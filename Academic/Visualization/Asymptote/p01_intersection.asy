size(5cm);
defaultpen(4);

pair z1=(0,0);
pair z2=(1,1);

real r=1;

path c1=circle(z1,r);
path c2=circle(z2,r);

// intersection area
picture it;
fill(it,c1,pink+purple);
clip(it,c2);
add(it);

//circles
draw(c1,pink);
draw(c2,purple);

// labels
label("$A$",z1);
label("$B$",z2);
real middle=1/2;
path g=(2,0)--(middle,middle);
draw(Label("$A\cap B$",0),g=g,p=currentpen+linewidth(0.8),arrow=Arrow);

// intersection points

for(pair p: intersectionpoints(c1,c2)){
  draw(p,red);
}
