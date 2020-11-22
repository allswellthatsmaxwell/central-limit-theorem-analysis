library(dplyr)
library(magrittr)
library(ggplot2)


###############################################################################
## Plot convolutions  #########################################################
###############################################################################
STEP <- 0.01
FROM = -2
TO = 2
steps <- seq(from = FROM, to = TO, by = STEP)

convolve_and_entibble <- function(steps, f, g, name) {
  conv <- convolve(f, g) 
  ft <- tibble::tibble(name, side = 'f', x = steps, y = f)
  gt <- tibble::tibble(name, side = 'g', x = steps, y = g)
  ct <- tibble::tibble(name, side = 'f * g', x = steps, y = convolve(f, g))
  dplyr::bind_rows(ft, gt, ct)
}

make_shape <- function(start, stop, shape_fn) {
  prop_along <- 1:length(steps) / length(steps)
  midpoint <- mean(c(start, stop))
  y <- 0
  draw <- function(x) {
    y <<- shape_fn(x, y)
    y
  }
  sapply(prop_along, function(x) {
    if (dplyr::between(x, start, stop)) draw(x) else x
  })
}
f1 <- dunif(steps, -1, 0)
start <- 0
stop <- 1
midpoint <- mean(c(start, stop))
triangle_shaper <- function(x, y) y + ifelse(x < midpoint, STEP, -STEP)
g1 <- make_shape(start, stop, triangle_shaper)
df1 <- convolve_and_entibble(steps, f1, g1, "A")

df1 %>%
  ggplot(aes(x = x, y = y, group = side)) +
  facet_wrap(~side, ncol = 1, scales = 'free_y') +
  geom_line() +
  theme_bw()
  

