# install.packages("patchwork") # required

library(dplyr)
library(magrittr)
library(ggplot2)
library(patchwork)

###############################################################################
## Plot convolutions  #########################################################
###############################################################################
STEP <- 0.001
FROM = -10
TO = 10
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
  conv <- convolve(f, g, type = TYPE, conj = FALSE)
  #conv <- pracma::conv(f, g)
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
  target_mean <- sum(steps * f) + sum(steps * g)
  print(sum(steps * conv))
  conv <- conv - sum(steps * conv)
  print(sum(steps * conv))
  conv <- conv + target_mean
  print(sum(steps * conv))
  ct <- tibble::tibble(name, side = '=', x = steps, y = conv)
  dplyr::bind_rows(ft, gt, ct)
}

make_shape_xi <- function(start, stop, shape_fn) {
  fn <- function(x) {
    if (dplyr::between(x, start, stop)) shape_fn(x) else 0
  }
  fn
}

post_shaper <- function(x) 4
steps1 <- steps
lower1 <- min(steps1)
upper1 <- max(steps1)
f1 <- make_shape(steps1, 0, 1, post_shaper) %>% normalize()
g1 <- f1#make_shape(steps1, 1, 1.5, post_shaper) %>% normalize()

#f1i <- Vectorize(function(x) ifelse(x >= -0.5 & x <= 0, 4, 0))
#g1i <- Vectorize(function(x) ifelse(x >= 1 & x <= 1.5, 4, 0))

#get_integral_body <- function(x, f, g) {
#  function(y) {
#    f(y) * g(x - y)
#  }
#}

#get_integral1 <- function(x) get_integral(
#  f = f1i, g = g1i, lower = lower1, upper = upper1)
#c1 <- sapply(steps1, function(x) get_integral(x = x, f = f1i, g = g1i, 
#                                              lower = lower1, upper = upper1))
#convolution1 <- steps %>%
#  lapply(function(x) get_integral_body(x, f1i, g1i)) %>%
#  sapply(function(fn) pracma::quad(fn, lower1, upper1))
#plot(steps1, convolution1)

#cc1 <- convolve(f1, g1, type = "open")
#plotc(c1)
name1 <- "uniform * uniform"
df1 <- convolve_and_entibble(steps1, f1, g1, name1)
#plot(pracma::conv(f1, g1))
plot_convolutions(df1)

myconv <- function(x, y) {
  lx <- length(x)
  ly <- length(y)
  n <-  lx +  ly - 1
  lpad <- n - lx
  rpad <- n - ly
  z <- fft(fft(c(x, rep(0, lpad))) * fft(c(y, rep(0, rpad))),
           inverse = TRUE) / n
  if (is.numeric(x) && is.numeric(y))
    z <- Re(z)
  ## remove padding
  z
  #slice <- 1:(length(z) - rpad)
  #print(slice)
  #z[slice]
}

convo <- function(x, f, g) {
  fog = 0
  xt = min(steps1)
  xend <- max(steps1)
  dx = step_size
  while (xt < xend) {
    fog = fog + f(xt)*g(x-xt)*dx
    xt = xt + dx
  }
  fog
}

ccc <- convolve(f1, g1, conj = FALSE, type = "open")
ccc <- sapply(steps1, function(x) convo(x, f1i, g1i))
step_size <- (steps[2] - steps[1])
cstart <- steps1[1] + steps1[1] ## first point for p1 + first point for p2
cend <- cstart + (length(f1) + length(g1) - 2) * step_size
cseq <- seq(from = cstart, to = cend, by = step_size)
cend - cstart
max(steps) - min(steps)
plot(cseq, ccc)
lines(steps1, f1); lines(steps1, g1)

sum(ccc == 0)

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

