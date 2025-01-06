library(tidyverse)

cr <- 0:10

g <- expand.grid(X = cr, Y = cr)
g$Z <- as.vector(outer(cr, cr, function(x, y) x * y))

ggplot(g, aes(x = X, y = Y, fill = Z)) +
  scale_x_continuous(n.breaks = 11, limits = c(-1, 11)) +
  scale_y_continuous(n.breaks = 11, limits = c(-1, 11)) +
  geom_tile()

plot(g$X, g$Y)
