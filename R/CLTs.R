# install.packages("patchwork") # required

library(dplyr)
library(magrittr)
library(ggplot2)
library(patchwork)

###############################################################################
## Plot convolutions  #########################################################
###############################################################################
STEP <- 0.001
FROM = -4
TO = 4
steps <- seq(from = FROM, to = TO, by = STEP)

#' Makes a shape along the range FROM to TO, that's 0 everywhere
#' except for between start and stop. Uses shape_fn to draw.
make_shape <- function(steps, start, stop, shape_fn) {
  sapply(steps, function(x) {
    if (dplyr::between(x, start, stop)) shape_fn(x) else 0
  })
}

normalize <- function(x) x / sum(x)

TYPE <- "circular"
#' Puts f, g, and convolve(f, g) into a long tibble.
convolve_and_entibble <- function(steps, f, g, name) {
  ft <- tibble::tibble(name, side = ' ', x = steps, y = f)
  gt <- tibble::tibble(name, side = '*', x = steps, y = g)
  conv <- convolve(f, g, type = TYPE)
  ## pad steps vector
  step_size <- steps[2] - steps[1]
  target_len <- length(conv)
  diff <- target_len - length(steps)
  halfdiff <- diff %/% 2
  #leftmost <- min(steps)
  #rightmost <- max(steps)
  #lpad <- seq(from = leftmost - step_size * halfdiff + step_size, to = leftmost, by = step_size)
  #rpad <- seq(from = rightmost, to = rightmost + step_size * halfdiff - step_size, by = step_size)
  #padded_steps <- c(lpad, steps, rpad)
  #print(length(conv))
  #print(length(padded_steps))
  ct <- tibble::tibble(name, side = '=', x = steps, y = conv)
  dplyr::bind_rows(ft, gt, ct)
}

post_shaper <- function(x) 4
steps1 <- steps
f1 <- make_shape(steps1, -0.5, 0, post_shaper) %>% normalize()
g1 <- make_shape(steps1, 1, 1.5, post_shaper) %>% normalize()
name1 <- "uniform * uniform"
df1 <- convolve_and_entibble(steps1, f1, g1, name1)

steps2 <- steps1
f2 <- f1 %>% normalize()
g2 <- convolve(f1, g1) %>% normalize()
name2 <- "uniform * (uniform * uniform)"
df2 <- convolve_and_entibble(steps2, f2, g2, name2)

steps3 <- seq(from = 0, to = 20, by = STEP * 20)
f3 <- dgamma(steps3, 7.5, 1) %>% normalize()
g3 <- dgamma(steps3, 2, 2) %>% normalize()
name3 <- "gamma * gamma"
df3 <- convolve_and_entibble(steps3, f3, g3, name3)

steps4 <- seq(from = 0, to = 1, by = STEP)
f4 <- dbeta(steps4, 2, 9) %>% normalize()
g4 <- dbeta(steps4, 9, 2) %>% normalize()
name4 <- "beta * beta"
df4 <- convolve_and_entibble(steps4, f4, g4, name4)

name5 <- "bimodal * bimodal"
steps5 <- seq(from = -10, to = 30, by = STEP * 40)
f5 <- normalize(make_shape(steps5, 16, 18, post_shaper) + 
                make_shape(steps5, 7, 10, post_shaper))
g5 <- normalize(f4 + g4)
df5 <- convolve_and_entibble(bisteps, f5, g5, name5)


plot_convolutions <- function(dat) {
  dat %>%
    ggplot(aes(x = x, y = y, group = side, color = side)) +
    facet_wrap(~side, ncol = 1) +
    scale_color_manual(values = c(' ' = 'blue', '*' = 'red', '=' = 'black')) +
    geom_line(size = 1.05) +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          #axis.text.y = element_blank(),
          #axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 19, vjust = 0.5),
          strip.text = element_text(size = 28, vjust = 0.5),
          strip.background = element_blank())
}

dfs <- list(df1, df2, df3, df4, df5)
plots <- lapply(dfs, plot_convolutions)
names <- c(name1, name2, name3, name4, name5)
plots <- Map(function(plot, name) plot + ggtitle(name),
             plots, names)
Reduce(`+`, plots) + plot_layout(nrow = 1)

