library(tidyverse)
somedata <- data.frame(xx = seq(1, 5 * pi, 0.1), yy = sin(seq(1, 5 * pi, 0.1)))

ggplot(data = somedata, mapping = aes(x = xx, y = yy)) +
  geom_line() +
  geom_point(color = "green")

ggsave("02.pdf")
