set terminal pdf
plot [0:4][] "data-simple" using 1:2 with lines, \
             "data-simple" using 1:3 with linespoints
