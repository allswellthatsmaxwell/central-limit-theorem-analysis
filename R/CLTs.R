library(dplyr)
library(magrittr)
library(ggplot2)

data_paths <- list('weights_heights' = '../data/weight-height.csv')
hist_plot_elements <- list(
  geom_histogram(bins = 60),
  theme_bw())
weights_heights_plot_elements <- list(
  labs(x = "Height, inches")) 
out_theme1 <- theme(axis.text = element_text(size = 16),
                    axis.title = element_text(size = 16))
wh_hists_elements <- hist_plot_elements %>%
  append(weights_heights_plot_elements) %>%
  append(list(out_theme1))


whdat <- readr::read_csv(data_paths[['weights_heights']])
true_mean <- mean(whdat$height)

## Show original data. #####
original_weights_plot <- whdat %>%
  ggplot(aes(x = height)) +
  wh_hists_elements

original_weights_plot

## Look at CLT for mean. #####
SAMPLE_SIZE <- 3000
SAMPLES <- 100

max_roll <- 50
n_experiments <- 600
elements_in_sample_vec <- sample(1:max_roll, n_experiments, replace=TRUE)
number_samples_vec <- sample(1:max_roll, n_experiments, replace=TRUE)

do_experiment <- function(elements_in_sample, number_samples) {
  sample_dats <- lapply(1:number_samples, 
                        function(i) whdat %>% dplyr::sample_n(elements_in_sample))
  sample_means <- sample_dats %>% sapply(function(d) mean(d$height))
}

experiments_sample_means <- Map(do_experiment, elements_in_sample_vec, number_samples_vec)
experiments_means_of_means <- sapply(experiments_sample_means, mean)
experiment_dat <- tibble::tibble(
  elements_in_sample = elements_in_sample_vec,
  number_samples = number_samples_vec,
  mean_of_sample_means = experiments_means_of_means)
experiment_dat

## Predict error using # elements and # samples as features. 
## Look at variable weights.

## Hold one fixed and vary the other, and plot that 2D graph.
## Actually you could probably derive this partial derivative of the error analytically!
get_partial_derivatives <- function(fixed_val, fixed_experiment_maker, name) {
  partial_experiment_function <- fixed_experiment_maker(fixed_val)
  xs <- 1:max_roll
  print(length(xs))
  means_of_means <- sapply(xs, partial_experiment_function)
  print(length(means_of_means))
  tibble::tibble(fixed_val = fixed_val, varying_val = xs, mean_of_means = means_of_means,
                 name = name)
}
fix_n_samples <- function(n_samples) {
  function(n_elements) dplyr::sample_n(whdat, n_elements) %$% mean(height)
}

get_partial_derivatives(400, fix_n_samples, 'medium fixed samples')
df <- get_partial_derivatives(400, fix_n_samples, 'medium fixed samples')

df %>%
  ggplot(aes(x = varying_val, y = abs(mean_of_means - true_mean))) +
  geom_point() +
  theme_bw() +
  out_theme1


library(rgl)
fn <- plot3D::points3D
fn <- rgl::rgl.points
rgl.open()
with(experiment_dat, fn(x = elements_in_sample, y = number_samples, 
                        z = mean_of_sample_means))

experiment_dat %>%
  ggplot(aes(x = elements_in_sample))

start_time <- Sys.time()
sample_dats <- lapply(1:MAX, function(i) whdat %>% dplyr::sample_n(MAX))
sample_means <- sample_dats %>% sapply(function(d) mean(d$height))
end_time <- Sys.time()
end_time - start_time
sample_means_dat <- tibble::tibble(mean_height = sample_means) 

## double integral of x/(2000 minutes, from 1 to 2000) * (y/2000 minutes from 1 to 2000 dy) 

sample_means_plot <- sample_means_dat %>%
  ggplot(aes(x = mean_height)) +
  wh_hists_elements
sample_means_plot
  
