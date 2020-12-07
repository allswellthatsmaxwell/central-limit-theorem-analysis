# install.packages("patchwork") # required

library(dplyr)
library(magrittr)
library(ggplot2)
library(patchwork)
library(tibble)
library(ggrepel)
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

###############################################################################
###############################################################################
#### Convergence rates for different distribution families. ###################
###############################################################################
###############################################################################

###############################################################################
## Setup. #####################################################################
###############################################################################
prod <- list(nframes = 200, fps = 30, start_pause = 80, height = 5, width = 10)
dev <- list(nframes = 100, fps = 20, start_pause = 10, height = 5, width = 10)
anim_settings <- prod

N <- 30
ALLOW_WRITE <- FALSE
xs <- seq(-10, 1000, 0.01)
fs <- list(
  "gamma1" = list(d = function(xs) dgamma(xs, shape = 2, scale = 2) %>% normalize(),
                  name = "gamma(2, 2)"),
  "gamma2" = list(d = function(xs) dgamma(xs, shape = 5, scale = 2) %>% normalize(),
                  name = "gamma(5, 2)"),
  
  
  "unif1" = list(d = function(xs) make_shape(xs, 1, 2, post_shaper) %>% normalize(),
                 name = "uniform(1, 2)"),
  
  ## Wow - this one has higher kurtosis and skew 
  ## after 1 to 3 or 4 or 5 convolutions than it does at 0.
  "bimodal1" = list(d = function(xs) 
    (make_shape(xs, 8, 12, post_shaper) + 
     make_shape(xs, -2, 2, post_shaper)) %>% normalize(),
    name = "bimodal(8, 12; -2, 2)"),
  
  "pareto1" = list(d = function(xs) dpareto(xs, 2) %>% normalize(),
                   name = "pareto(2)"),
  
  "exp1" = list(d = function(xs) dexp(xs, 20) %>% normalize(),
                name = "exp(20)"),
  
  "exp2" = list(d = function(xs) dexp(xs, 3) %>% normalize(),
                name = "exp(3)"),
  
  "gaussian1" = list(d = function(xs) dnorm(xs, 10, 3) %>% normalize(),
                     name = "gaussian(10, 3)"),
  
  "beta1" = list(d = function(xs) dbeta(xs, 20, 10) %>% normalize(),
                 name = "beta(20, 10)"),
  
  "beta2" = list(d = function(xs) dbeta(xs, 50, 1) %>% normalize(),
                 name = "beta(50, 1)"),
  
  "beta3" = list(d = function(xs) dbeta(xs, 100, 10) %>% normalize(),
                 name = "beta(100, 10)"),
  
  "cauchy1" = list(d = function(xs) dcauchy(xs, 0, 2),
                   name = "cauchy(0, 2)")
)
for (distname in names(fs)) {
  fs[[distname]][["xs"]] <- xs
}


dists_to_gif <- c("bimodal1", "beta2", "unif1", "exp1", "gaussian1")
names(dists_to_gif) <- dists_to_gif

descriptive_names_df <- tibble(
  name = names(fs), display_name = sapply(fs, function(fdict) fdict %$% name))

long_moments_df <- fs %>% 
  make_long_moments_df(N) %>%
  dplyr::left_join(descriptive_names_df, by = c("distribution" = "name"))

distribution_colors <- long_moments_df %>%
  dplyr::filter(!(distribution %in% toughies)) %>%
  get_distributions_colors()

target_moment <- "kurtosis"


###############################################################################
## Plotting relationship between number of convolutions and kurtosis. #########
###############################################################################
kurtosis_by_convolutions_plot <- long_moments_df %>%
  dplyr::filter(moment_name == target_moment,
                distribution %in% dists_to_gif) %>%
  make_moment_by_convolutions_plot()

if (ALLOW_WRITE) {
  ggsave(plot = kurtosis_by_convolutions_plot, path = '../plots', 
         filename = "kurtosis_by_convolutions.png",
         dpi = 200, width = 8.06, height = 7.36, units = "in")
}
###############################################################################
## Plotting relationship between skew and kurtosis. ###########################
###############################################################################
sk_relationship_df <- get_sk_relationship(long_moments_df, N)

skew_vs_kurtosis_plot <- make_skew_vs_kurtosis_plot(
  sk_relationship_df, descriptive_names_df, distribution_colors)  
skew_vs_kurtosis_plot


if (ALLOW_WRITE) {
  ggsave(plot = skew_vs_kurtosis_plot, path = '../plots', filename = "skew_vs_kurtosis.png",
         dpi = 300, width = 10.75, height = 6.375, units = "in")
}
###############################################################################
## Making the animations. #####################################################
###############################################################################
## Test animation.
gif <- fs[["exp1"]] %>% animate_convolutions(prod)
if (ALLOW_WRITE) {
  gganimate::anim_save(glue::glue("../plots/exp1.gif"), gif)
}

## Actual all animations.
if (ALLOW_WRITE) {
  for (distribution_name in dists_to_gif) {
    gifplot <- fs[[distribution_name]] %>% animate_convolutions(prod)
    gganimate::anim_save(glue::glue("../plots/{distribution_name}.gif"), gifplot)
  }
}

###############################################################################
###############################################################################
## Convolving different shapes. ###############################################
###############################################################################
###############################################################################

make_beta <- function(xs, trials, prop_success) {
  success <- round(trials * prop_success)
  fail <- trials - success
  alpha <- success + 1
  beta <- fail + 1
  ys <- dbeta(xs, alpha, beta)
  name <- glue::glue("beta({alpha}, {beta})")
  tibble::tibble(xs, ys, alpha, beta, name)
}

#' returns k beta distributions.
beta_generator <- function(xs, k, seed = 5) {
  trials_max <- 10
  successes_prop_max <- 1
  trials_list <- sample(trials_max, k)
  success_prop_list <- runif(k, min = 0, max = successes_prop_max)
  set.seed(seed)
  Map(function(t, s) make_beta(xs, t, s), trials_list, success_prop_list) %>%
    dplyr::bind_rows()
}
betas_df <- beta_generator(xs, 5)

betas_df %>% 
  dplyr::filter(dplyr::between(xs, 0, 1)) %>%
  ggplot(aes(x = xs, y = ys, color = name)) +
  geom_line() +
  theme_bw() 
  #facet_wrap(~name, scales = "free_y")
  
  

###############################################################################
###############################################################################
### Other. ####################################################################
###############################################################################
###############################################################################
f <- 0.0001
M <- 1000
hist(sapply(1:10000, function(i) sum(runif(M) <= f)))
M*f
