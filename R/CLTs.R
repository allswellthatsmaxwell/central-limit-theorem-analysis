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

## Show original data. #####
original_weights_plot <- whdat %>%
  ggplot(aes(x = height)) +
  wh_hists_elements

original_weights_plot

## Look at CLT for mean. #####
SAMPLE_SIZE <- 3000
SAMPLES <- 100

MAX <- 1500
STEP <- 20

max_roll <- 3000
n_experiments <- 50
elements_in_sample_vec <- sample(1:max_roll, n_experiments, replace=TRUE)
number_samples_vec <- sample(1:max_roll, n_experiments, replace=TRUE)

do_experiment <- function(elements_in_sample, number_samples) {
  sample_dats <- lapply(1:number_samples, 
                        function(i) whdat %>% dplyr::sample_n(elements_in_sample))
  sample_means <- sample_dats %>% sapply(function(d) mean(d$height))
}

experiment_means_of_means <- Map(do_experiment, elements_in_sample_vec, number_samples_vec)

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
  
