size(5cm);
unitsize(1cm);

// despite p1 uses scale, the end picture is still 1cm;
picture p1;
size(p1,1cm);
fill(p1,scale(2)*unitsquare,lightgreen);

// despite p2's square is drawn to unitsize, it will be scaled to 2cm;
picture p2;
size(p2,2cm);
fill(p2,unitsquare,pink);

add(p1.fit(),(0,0));
add(p2.fit(),(1,1));
