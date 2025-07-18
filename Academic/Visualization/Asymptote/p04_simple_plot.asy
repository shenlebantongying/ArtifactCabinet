import graph;

size(20cm,10cm,IgnoreAspect);
real mySin(real x);

mySin=new real(real x){
  return sin(x);
};

pen lineDrawPen=linewidth(0.05cm);
draw(graph(mySin,-2pi,2pi),lineDrawPen+red+dashed,Label("MySin",black));
draw(graph(new real(real x){return mySin(x+pi);},-2pi,2pi),
     lineDrawPen+blue+dotted,
     Label("NewSin",black));

yaxis(LeftRight,-2,2,Ticks);
yaxis(-2,2,Ticks(" "));

// TODO: better way ?
xaxis(" ",BottomTop,Ticks(" ",Step=pi,step=pi/2));
for(int i=-2;i<=2;++i){
  xtick(Label(format("$%d\pi$",i)),(i*pi,-2.2));
};

picture leg;
add(leg,legend(xmargin=2.0),UnFill);

picture title;
label(title,Label("Basic Plot"),(0,0),p=fontsize(1cm));

add(leg.fit(),(2pi,2),10SW);
add(title.fit(),(0,2),10N);
