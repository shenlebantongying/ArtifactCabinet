unitsize(1cm);
defaultpen(linewidth(0.1cm));

int d=60; // degree

pair[] dps; // data points

for(int i=0; i<360/d+1; ++i){
  // dir(degree) returns in unit circle, scale to bigger size
  pair p = dir(d*i)*2;
  dps.push(p);
}

path s =(0,0);
for(pair t: dps){
  s=s--t--(0,0)--t;
}

draw(s,pink);

for(pair t: dps){
  fill(circle(t,1/20),purple);
}
