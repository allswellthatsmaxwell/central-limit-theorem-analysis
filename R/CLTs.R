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

#' Makes a shape along the range FROM to TO, that's 0 everywhere
#' except for between start and stop. Uses shape_fn to draw.
make_shape <- function(start, stop, shape_fn) {
  prop_along <- 1:length(steps) / length(steps)
  y <- 0
  draw <- function(x) {
    y <<- shape_fn(x, y)
    y
  }
  sapply(steps, function(x) {
    if (dplyr::between(x, start, stop)) draw(x) else x
  })
}

#' Puts f, g, and convolve(f, g) into a long tibble.
convolve_and_entibble <- function(steps, f, g, name) {
  conv <- convolve(f, g) 
  ft <- tibble::tibble(name, side = 'f', x = steps, y = f)
  gt <- tibble::tibble(name, side = 'g', x = steps, y = g)
  ct <- tibble::tibble(name, side = 'z = f * g', x = steps, 
                       y = convolve(f, g, type = 'circular'))
  dplyr::bind_rows(ft, gt, ct)
}

f1 <- dunif(steps, -1, 0)
start <- 0.4
stop <- 0.6
midpoint <- mean(c(start, stop))
triangle_shaper <- function(x, y) y + ifelse(x < midpoint, STEP, -STEP)
post_shaper <- function(x, y) 4

g1 <- make_shape(start, stop, triangle_shaper)
df1 <- convolve_and_entibble(steps, f1, g1, "A")

f2 <- g2 <- make_shape(start, stop, post_shaper)
df2 <- convolve_and_entibble(steps, f2, g2, "posts")


df1 %>%
  ggplot(aes(x = x, y = y, group = side, color = side)) +
  facet_wrap(~side, ncol = 1, scales = 'free_y') +
  scale_color_manual(values = c('f' = 'blue', 'g' = 'red', 'z = f * g' = 'black')) +
  geom_line() +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
  

