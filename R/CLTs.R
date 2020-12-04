# install.packages("patchwork") # required

library(dplyr)
library(magrittr)
library(ggplot2)
library(patchwork)
library(tibble)
library(gapminder)
library(gifski)
library(gganimate)
library(EnvStats)
source("clt_functions.R")

###############################################################################
## Plot convolutions  #########################################################
###############################################################################
STEP <- 0.001
FROM = 0
TO = 16
steps <- seq(from = FROM, to = TO, by = STEP)


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



prod <- list(nframes = 200, fps = 50, start_pause = 80)
dev <- list(nframes = 100, fps = 20, start_pause = 10)
anim_settings <- dev

N <- 60
xs <- seq(-10, 1000, 0.1)
fs <- list(
  "gamma1" = list(d = function(xs) 
    dgamma(xs, shape = 1, scale = 2) %>% normalize()),
  "gamma3" = list(d = function(xs) 
    dgamma(xs, shape = 2, scale = 2) %>% normalize()),
  "unif1" = list(d = function(xs) 
    make_shape(xs, 1, 2, post_shaper) %>% normalize()),
  
  ## Wow - this one has higher kurtosis and skew 
  ## after 1 to 3 or 4 or 5 convolutions than it does at 0.
  "bimodal1" = list(d = function(xs) 
    (make_shape(xs, 8, 12, post_shaper) + 
     make_shape(xs, -2, 2, post_shaper)) %>% normalize()),
  "pareto1" = list(d = function(xs) 
    dpareto(xs, 2) %>% normalize()),
  "exp1" = list(d = function(xs) 
    dexp(xs, 10) %>% normalize()),
  "gaussian1" = list(d = function(xs) 
    dnorm(xs, 10, 3) %>% normalize())
  )
for (distname in names(fs)) {
  fs[[distname]][["xs"]] <- xs
}

distname <- "bimodal1"
distname <- "exp1"
distname <- "gamma1"
distname <- "gaussian1"

dx <- xs[2] - xs[1]
fs[[distname]] %>%
  apply_convs_and_stack() %>% 
  group_by(convolutions) %>%
  dplyr::summarize(mass = sum(h),
                   mean = sum(xs * h * dx),
                   variance = sum(xs^2 * h * dx),
                   skew = sum(xs^3 * h * dx),
                   kurtosis = sum(xs^4 * h * dx))

fs[[distname]] %>%
  apply_convs_and_stack() %>% 
  group_by(convolutions) %>%
  dplyr::summarize(mass = sum(h),
                   mean = mean(h / dx),
                   variance = sd(h)^2,
                   skew = moments::skewness(h),
                   kurtosis = moments::kurtosis(h))


fs[[distname]] %>%
  get_moments()

fs[[distname]] %>%
  get_moments() %>%
  ggplot(aes(x = convolutions, y = val, color = statistic)) +
  geom_point() +
  geom_line() +
  theme_bw()

fs[[distname]][['anim']] <- fs[[distname]] %>%
  apply_convs_then_plot() %>%
  {with(anim_settings, {
    gganimate::animate(., start_pause = start_pause, fps = fps, 
                       nframes = nframes)})}
fs[[distname]][["anim"]]


gganimate::anim_save(glue::glue("{distname}.gif"), gifplot)
# magick::image_write(anim, path="myanimation.gif")
# dfs %>% lapply(get_probability_greater)
# facet_wrap(~convolutions, scales = "free", ncol = 1) +


compare_dat <- fs[["gamma1"]][["compare"]]
compare_dat %$% ks.test(x = h, y = ideal_gaussian) %$% statistic


d <- fs[["gamma1"]][["d"]]
xs <- fs[["gamma1"]][["xs"]]

d(xs)


sum(xs * d(xs))
plot(xs, d(xs))

r <- convolve_n_times(fs[["gamma1"]], N)

sum(xs * r)






