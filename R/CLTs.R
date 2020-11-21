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
fix_n_samples <- function(n_samples) {
  function(n_elements) 
    sapply(1:n_samples, 
           function(i) dplyr::sample_n(whdat, n_elements) %$% mean(height))
}
fix_n_elements <- function(n_elements) {
  function(n_samples)
    dplyr::sample_n(whdat, n_elements) %$% mean(height)
}


## Hold one fixed and vary the other, and plot that 2D graph.
## Actually you could probably derive this partial derivative of the error analytically!
get_partial_derivatives <- function(
  fixed_n_elements, n_samples_range, name, n_takes_for_variance = 30) {
  ## one element per sample size we're trying.
  ## if we do this many times for each # of samples (do it a few times for 1 sample, do it a few times for 2 samples...),
  ## then take the variances, we'll have the partial derivative of variance w.r.t. the # of samples.
  avg_variances_in_mean_estimation <- sapply(
    n_samples_range, ## for each size in the range of number-of-samples
    function(i) {
      mean_of_means_of_sample_means <- sapply(
        1:n_takes_for_variance, ## for all the takes we want to estimate our variance
        function(take_j) {
          mean_of_sample_means <- sapply(
            n_samples_range, ## for every sample we're going to do
            function (n_samples) {
              ## for every sample
              sample_means <- sapply(1:n_samples, 
                     function(k) {
                       sample <- dplyr::sample_n(whdat, fixed_n_elements) %$% height
                       mean(sample) ## sample mean
               })
              mean(sample_means) ## mean of sample means
            })
          mean(mean_of_sample_means) ## variance of different mean of sample means
        })
      var(mean_of_means_of_sample_means)
    })
  tibble::tibble(name, n_samples_range, avg_variances_in_mean_estimation)
}

df <- get_partial_derivatives(30, 1:20, 'medium fixed samples')

df %<>% mutate(error = mean_of_means - true_mean)

df %>%
  ggplot(aes(x = n_samples_range, y = avg_variances_in_mean_estimation)) +
  geom_point() +
  theme_bw() +
  out_theme1

df %>%
  ggplot(aes(x = error)) +
  geom_histogram(bins = 60) +
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
  
