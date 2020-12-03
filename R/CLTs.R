# install.packages("patchwork") # required

library(dplyr)
library(magrittr)
library(ggplot2)
library(patchwork)
library(tibble)
library(gapminder)
library(gifski)
library(gganimate)
source("clt_functions.R")

###############################################################################
## Plot convolutions  #########################################################
###############################################################################
STEP <- 0.001
FROM = 0
TO = 16
steps <- seq(from = FROM, to = TO, by = STEP)


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




N <- 100
xs <- seq(-10, 1000, 0.1)
fs <- list(
  "gamma2" = list(
    d = function(xs) dgamma(xs, shape = 3, scale = 0.5) %>% normalize(),
    xs = xs),
  "gamma1" = list(
  d = function(xs) dgamma(xs, shape = 1, scale = 2) %>% normalize(),
  xs = xs))
plot(fs$gamma1$d(xs[1:300]))
# fs[["gamma1"]][["compare"]] <- bundle_n_convolutions(fs[["gamma1"]], N)
## how close is h to a gaussian with the same first and second moments?

dfs <- fs %$% 
  gamma1 %>% 
  convolve_n_times(N) %>%
  lapply(function(df) attach_ideal_gaussian(df, sds = 4))

combined_df <- dplyr::bind_rows(dfs)

dfs %>% lapply(get_probability_greater)

SIZE <- 7
base_len <- 0.1
len_factor <- 4
ymax <- with(combined_df, max(h, ideal_gaussian))
gifplot <- combined_df %>%
  dplyr::filter(convolutions <= 30) %>%
  ggplot(aes(x = x, group = convolutions)) +
  geom_line(aes(y = ideal_gaussian), color = 'black', alpha = 1, size = SIZE) +
  geom_line(aes(y = h), color = '#F8766D', alpha = 0.95, size = SIZE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  # facet_wrap(~convolutions, scales = "free", ncol = 1)
  labs(title = "convolutions: {closest_state}") +
  gganimate::transition_states(convolutions, wrap = FALSE,
                               transition_length = base_len / len_factor, 
                               state_length = base_len / len_factor) +
  gganimate::ease_aes('linear') +
  view_follow(fixed_x = FALSE,
              fixed_y = FALSE)
# gifplot
  
anim <- gganimate::animate(gifplot, start_pause = 10)
anim

gganimate::anim_save("gamma1.gif", gifplot)
# magick::image_write(anim, path="myanimation.gif")

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






