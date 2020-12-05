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

prod <- list(nframes = 200, fps = 30, start_pause = 80, height = 5, width = 10)
dev <- list(nframes = 100, fps = 20, start_pause = 10, height = 5, width = 10)
anim_settings <- prod

N <- 30
xs <- seq(-10, 1000, 0.01)
fs <- list(
  #"gamma1" = list(d = function(xs) 
  #  dgamma(xs, shape = 1, scale = 2) %>% normalize()),
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
    dnorm(xs, 10, 3) %>% normalize()),
  #"beta1" = list(d = function(xs) 
  #  dbeta(xs, 0.5, 0.5) %>% normalize()),
  "beta2" = list(d = function(xs) 
    dbeta(xs, 50, 1) %>% normalize())
  )
for (distname in names(fs)) {
  fs[[distname]][["xs"]] <- xs
}

distname <- "bimodal1"
distname <- "exp1"
distname <- "gamma1"
distname <- "gaussian1"

add_name <- function(dat, name) {
  dat %>% mutate(distribution = name)
}
long_moments_df <- fs %>%
  lapply(function(fdict) get_moments(fdict, N)) %>%
  {Map(add_name, ., names(.))} %>% 
  lapply(stack_moments) %>%
  dplyr::bind_rows()

target_moment <- "excess_kurtosis"
long_moments_df %>%
  dplyr::filter(moment_name == target_moment, distribution != 'pareto1') %>%
  ggplot(aes(x = convolutions, y = abs(moment), color = distribution)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, max(long_moments_df$convolutions), by = 2)) +
  scale_y_continuous(breaks = seq(0, max(long_moments_df$moment), by = 1)) +
  labs(x = target_moment)

skew_to_start_df <- long_moments_df %>%
  dplyr::filter(moment_name == 'skew', convolutions == 0) %>%
  select(-moment_name) %>% ## (what about kurtosis to start?)
  rename(skew = moment)

kurtosis_at_end_df <- long_moments_df %>%
  dplyr::filter(moment_name == 'kurtosis', convolutions == N) %>%
  select(-moment_name) %>%
  rename(kurtosis = moment)

sk_relationship_df <- dplyr::inner_join(
  skew_to_start_df, kurtosis_at_end_df, by = "distribution",
  suffix = c("_skew", "_kurtosis"))
sk_relationship_df

sk_relationship_df %>%
  ggplot(aes(x = abs(skew), y = kurtosis, color = distribution)) +
  geom_point() +
  theme_bw() 
)  
  

dists_to_gif <- c("bimodal1", "beta2", "unif1")
names(dists_to_gif) <- dists_to_gif
stacks <- lapply(dists_to_gif, function(name) apply_convs_and_stack(fs[[name]]))

stack_to_gif_df <- stacks %>% 
  {Map(add_name, ., names(.))} %>%
  dplyr::bind_rows()
facet_gif <- stack_to_gif_df %>%
  make_plot_to_animate()

facet_gif %>% animate_plot(prod)

gif <- fs[["pareto1"]] %>% animate_convolutions(prod)

# gganimate::anim_save(glue::glue("../plots/beta2.gif"), gif)


for (distribution_name in dists_to_gif) {
  gifplot <- fs[[distribution_name]] %>% animate_convolutions(prod)
  gganimate::anim_save(glue::glue("../plots/{distribution_name}.gif"), gifplot)
}
# magick::image_write(anim, path="myanimation.gif")
# dfs %>% lapply(get_probability_greater)
# facet_wrap(~convolutions, scales = "free", ncol = 1) +

