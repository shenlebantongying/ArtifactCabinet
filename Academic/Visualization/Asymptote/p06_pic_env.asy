/*
    Draw With functions, and transform via pic enviroment.
*/
import markers;

size(40cm);

void draw_coord(picture pic) {
  draw(pic, (0, 0)--(0, 10), Arrow);
  draw(pic, (0, 0)--(10, 0), Arrow);
  draw(pic, (0, 0)--(0, -10), dashed);
  draw(pic, (0, 0)--(-10, 0), dashed);
}

picture pic1;
draw_coord(pic1);

picture pic;
int v = 6;
for (int i = 0; i < v + 1; i += 1) {
  add(shift(22 * i, 0) * rotate(-i * 360 / v) * pic1);
}
