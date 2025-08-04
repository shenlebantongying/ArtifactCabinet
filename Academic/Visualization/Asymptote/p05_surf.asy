settings.render = 16;
import three;
size(10cm);

draw(O -- X,red,L=Label("X"));
draw(O -- Y,blue,L=Label("Y"));
draw(O -- Z,green,L=Label("Z"));

draw(X -- Y -- Z..cycle);

draw(box(O, (1,1,1)));

draw(O -- (X+Y+Z),pink,L=Label("i"));
