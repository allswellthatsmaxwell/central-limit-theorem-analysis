# install.packages("patchwork") # required

library(dplyr)
library(magrittr)
library(ggplot2)
library(patchwork)

###############################################################################
## Plot convolutions  #########################################################
###############################################################################
STEP <- 0.001
FROM = 0
TO = 16
steps <- seq(from = FROM, to = TO, by = STEP)

#' Makes a shape along the range FROM to TO, that's 0 everywhere
#' except for between start and stop. Uses shape_fn to draw.
make_shape <- function(steps, start, stop, shape_fn) {
  sapply(steps, function(x) {
    if (dplyr::between(x, start, stop)) shape_fn(x) else 0
  })
}

normalize <- function(x) x / sum(x)

shift <- function(x, n) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

iterate_convolutions <- function(times, steps, f, g, name, shift_mean = TRUE) {
  times <- times - 1
  fg_df <- convolve_and_entibble(steps, f, g, name, shift_mean)
  fg_temp <- fg_df
  name_orig <- "f * g"
  name_temp <- name_orig
  l <- lapply(1:times, function(i) {
    fg_temp <<- convolve_and_entibble(
      steps, f = f, g = fg_temp %>% dplyr::filter(side == '=') %$% y, 
      name = name)
    fg_temp
  })
  lnames <- lapply(1:times, function(i) {
    name_temp <<- glue::glue("f * {name_temp}")
    name_temp
  })
  l <- append(list(fg_df), l)
  names(l) <- append(list(name_orig), lnames)
  l
}

make_iterated_plots <- function(iterated_dfs, plot_additions = list()) {
  iterated_plots <- iterated_dfs %>%
    lapply(plot_convolutions)
  iterated_plots <- Map(function(p, name) p + ggtitle(name) + plot_additions, 
                        iterated_plots, names(iterated_plots))
  iterated_plots %>% 
  {Reduce(`+`, .)} + plot_layout(nrow = 1)
}


plot_convolutions <- function(dat) {
  dat %>%
    ggplot(aes(x = x, y = y, group = side, color = side)) +
    facet_wrap(~side, ncol = 1) +
    scale_color_manual(values = c(' ' = 'blue', '*' = 'red', '=' = 'black')) +
    geom_line(size = 1.4) +
    theme_bw() +
    scale_x_continuous(breaks = 0, labels = "0") +
    theme(panel.grid = element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 20, vjust = 0.5),
          strip.text = element_text(size = 28, vjust = 0.5),
          strip.background = element_blank())
}

shift_convolution2 <- function(conv, steps, f, g) {
  current_mean <- sum(steps * conv)
  target_mean <- sum(steps * f) + sum(steps * g)
  conv / (current_mean / target_mean)
}

shift_convolution <- function(conv, steps, f, g) {
  step_size <- steps[2] - steps[1]
  current_mean <- sum(steps * conv)
  target_mean <- sum(steps * f) + sum(steps * g)
  distance <- target_mean - current_mean
  steps_in_distance <- round(distance / step_size)
  conv <- shift(conv, steps_in_distance) %>% normalize()
  rnd <- purrr::partial(round, digits = 3)
  conv_mean <- sum(steps * conv)
  print(glue::glue("attempted to move from mean {rnd(current_mean)} to {rnd(target_mean)} \
(a distance of {rnd(distance)}, with {steps_in_distance} steps); \
achieved {rnd(conv_mean)}."))
  conv
}

TYPE <- "circular"
#' Puts f, g, and convolve(f, g) into a long tibble.
convolve_and_entibble <- function(steps, f, g, name, shift_mean = TRUE) {
  ft <- tibble::tibble(name, side = ' ', x = steps, y = f)
  gt <- tibble::tibble(name, side = '*', x = steps, y = g)
  conv <- convolve(f, g, type = TYPE, conj = FALSE)
  if (shift_mean) {
    conv %<>% shift_convolution(steps, f, g)
  }
  print(glue::glue("{sum(steps * f)} + {sum(steps * g)} = {sum(steps * conv)}"))
  ct <- tibble::tibble(name, side = '=', x = steps, y = conv)
  dplyr::bind_rows(ft, gt, ct)
}

post_shaper <- function(x) 4
steps1 <- steps
f1 <- make_shape(steps1, 1, 2, post_shaper) %>% normalize()
g1 <- make_shape(steps1, 2.5, 3.5, post_shaper) %>% normalize()
name1 <- "unif * unif"
df1 <- convolve_and_entibble(steps1, f1, g1, name1)
#plot(pracma::conv(f1, g1))
plot_convolutions(df1)

make_unif <- function(s, a, b) make_shape(s, a, b, post_shaper) %>% normalize()
convolve_and_entibble_unifs <- function(from, to, a, b, c, d) {
  s <- seq(from = from, to = to, by = STEP)
  convolve_and_entibble(
    s, 
    f = make_unif(s, a, b), 
    g = make_unif(s, c, d),
    name1)
}
df1_negatives <- convolve_and_entibble_unifs(-30, 0, -10, -7, -15, -12)
df1_mixeds <- convolve_and_entibble_unifs(-15, 15, -4, -2, 10, 12)
plot_convolutions(df1_negatives) + plot_convolutions(df1_mixeds)


steps2 <- steps1
f2 <- f1 %>% normalize()
g2 <- df1 %>% dplyr::filter(side == '=') %$% y
name2 <- "unif * (unif * unif)"
df2 <- convolve_and_entibble(steps2, f2, g2, name2)



unif_iterated_dfs <- iterate_convolutions(5, steps1, f1, g1, name2)
unif_iterated_plots <- unif_iterated_dfs %>% make_iterated_plots() 
unif_iterated_plots


steps3 <- seq(from = 0, to = 20, by = STEP * 20)
f3 <- dgamma(steps3, 7.5, 1) %>% normalize()
g3 <- dgamma(steps3, 2, 2) %>% normalize()
name3 <- "gamma * gamma"
df3 <- convolve_and_entibble(steps3, f3, g3, name3)

steps4 <- seq(from = -0.5, to = 1.5, by = STEP)
f4 <- dbeta(steps4, 2, 9) %>% normalize()
g4 <- dbeta(steps4, 9, 2) %>% normalize()
name4 <- "beta * beta"
df4 <- convolve_and_entibble(steps4, f4, g4, name4)

name5 <- "bimodal * bimodal"
steps5 <- seq(from = -50, to = 50, by = STEP * 100)
f5 <- normalize(make_shape(steps5, 13, 20, post_shaper) + 
                make_shape(steps5, 30, 40, post_shaper))
g5 <- normalize(make_shape(steps5, -30, -20, post_shaper) + 
                make_shape(steps5, -11, -8, post_shaper))
f5 <- normalize(make_shape(steps5, 16, 18, post_shaper) + 
                  make_shape(steps5, 7, 10, post_shaper))
g5 <- normalize(make_shape(steps5, 12, 18, post_shaper) + 
                  make_shape(steps5, 4, 11, post_shaper))
df5 <- convolve_and_entibble(steps5, f5, g5, name5)

steps6 <- seq(from = -4, to = 4, by = STEP)
f6 <- dcauchy(-1)
g6 <- dcauchy(3)
df6 <- convolve_and_entibble(steps6, f6, g6, "cauchy")
plot_convolutions(df6)


bimodal_iterated_dfs <- iterate_convolutions(8, steps5, f5, g5, name5, shift_mean = FALSE)
bimodal_iterated_plots <- bimodal_iterated_dfs %>% 
  make_iterated_plots(list(theme(axis.text.x = element_blank(), 
                                 axis.ticks.x = element_blank())) )
bimodal_iterated_plots

dfs <- list(df1, df2, df3, df4, df5)
plots <- lapply(dfs, plot_convolutions)
names <- c(name1, name2, name3, name4, name5)
plots <- Map(function(plot, name) plot + ggtitle(name),
             plots, names)
five_plot <- Reduce(`+`, plots) + plot_layout(nrow = 1)

ggsave(five_plot, path = '../out', filename = 'five.png',
       dpi = 200, width = 12, height = 8, units = "in")


####
#### Convergence rates for different distribution families.
####


#bundle_n_convolutions <- function(fdict, n, sds = 4) {
#  h <- convolve_n_times(fdict, n)
#  xs <- fdict[["xs"]]
#  hmean <- sum(xs * h)
#  hsd <- sqrt(sum(((xs - hmean)^2) * h))
#  
#  xs <- fdict[["xs"]]
#  clipped_inds <- (xs > hmean - hsd * sds) & (xs < hmean + hsd * sds)
#  
#  xs <- xs[clipped_inds]
#  h <- h[clipped_inds]
#  ideal_gaussian <- dnorm(xs, hmean, hsd) %>% normalize()
#  
#  tibble::tibble(x = xs, h, ideal_gaussian)
#}


convolve_n_times <- function(fdict, n) {
  xs <- fdict[["xs"]]
  f <- fdict[["d"]](xs) %>% normalize()
  g <- f
  dfs <- vector(mode = "list", length = n)
  dfs[[1]] <- tibble::tibble(xs = xs, h = g, convolutions = 0)
  if (n > 1) {
    for (i in seq(from = 1, to = n, by = 1)) {
      g_prev <- g
      g <- convolve(g, f, conj = FALSE, type = "circular")
      dfs[[i]] <- tibble::tibble(xs = xs, h = g, convolutions = i)
    }
  }
  dfs
}

attach_ideal_gaussian <- function(df, sds = 10) {
  xs <- df$xs
  h <- df$h
  convolutions <- unique(df$convolutions)
  assertthat::are_equal(length(convolutions), 1)
  hmean <- sum(xs * h)
  hsd <- sqrt(sum(((xs - hmean)^2) * h))
  clipped_inds <- (xs > hmean - hsd * sds) & (xs < hmean + hsd * sds)
  xs <- xs[clipped_inds]
  h <- h[clipped_inds]
  ideal_gaussian <- dnorm(xs, hmean, hsd) %>% normalize()
  tibble::tibble(x = xs, h, ideal_gaussian, convolutions) 
}

get_tail_area <- function(h1, gs) {
  sum(gs > h1) / length(gs)
}

get_probability_greater <- function(dat) {
  h <- dat %$% h
  gauss <- dat %$% ideal_gaussian
  xs <- dat %$% xs
  length_h <- length(h)
  total <- 0
  while(length(h) > 0) {
    h1 <- h[1]
    matches <- h == h1
    count_h <- sum(matches)
    ph <- count_h / length_h
    g_tail_area <- get_tail_area(h1, gauss)
    total <- total + ph * g_tail_area
    h <- h[!matches]
  }
  total
}


N <- 10
xs <- seq(-10, 1000, 0.1)
fs <- list("gamma1" = list(
  d = function(xs) dgamma(xs, shape = 1, scale = 2) %>% normalize(),
  xs = xs))
with(fs %$% gamma1, plot(xs[1:1000], d(xs[1:1000])))
# fs[["gamma1"]][["compare"]] <- bundle_n_convolutions(fs[["gamma1"]], N)
## how close is h to a gaussian with the same first and second moments?

dfs <- fs %$% 
  gamma1 %>% 
  convolve_n_times(N) %>%
  lapply(attach_ideal_gaussian)

combined_df <- dplyr::bind_rows(dfs)

dfs %>% lapply(get_probability_greater)

ALPHA <- 0.6
SIZE <- 2
combined_df %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = h), color = 'purple', alpha = 0.8, size = SIZE) +
  geom_line(aes(y = ideal_gaussian), color = 'black', alpha = ALPHA / 2, size = SIZE) +
  # facet_wrap(~convolutions, scales = "free", ncol = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = 'convolutions: {frame_time}') +
  gganimate::transition_states(convolutions, transition_length = 2, state_length = 1) +
  gganimate::ease_aes('linear')


compare_dat <- fs[["gamma1"]][["compare"]]
compare_dat %$% ks.test(x = h, y = ideal_gaussian) %$% statistic


d <- fs[["gamma1"]][["d"]]
xs <- fs[["gamma1"]][["xs"]]

d(xs)


sum(xs * d(xs))
plot(xs, d(xs))

r <- convolve_n_times(fs[["gamma1"]], N)

sum(xs * r)






