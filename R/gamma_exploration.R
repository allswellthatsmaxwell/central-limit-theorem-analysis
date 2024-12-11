library(magrittr)


get_basic_no_hyperparameters_ratio <- function(data) {
  posterior <- list(alpha = data$alpha + data$y, 
                    beta = data$beta + data$N)
  posterior_mean <- posterior$alpha / posterior$beta
  prior_mean <- data$alpha / data$beta
  posterior_mean / prior_mean
}

get_s_ratio <- function(data) {
  s = min(c(20, data$beta))
  data$alpha <- s * (data$alpha / data$beta)
  data$beta <- s
  posterior <- list(alpha = data$alpha + data$y, 
                    beta = data$beta + data$N)
  posterior_mean <- posterior$alpha / posterior$beta
  prior_mean <- data$alpha / data$beta
  posterior_mean / prior_mean
}

get_s_k_ratio <- function(data) {
  k = 1
  prior_mean <- data$alpha / data$beta
  s = k / prior_mean
  data$alpha <- s * prior_mean
  data$beta <- s
  posterior <- list(alpha = data$alpha + data$y, 
                    beta = data$beta + data$N)
  posterior_mean <- posterior$alpha / posterior$beta
  posterior_mean / prior_mean
}

get_s_k_m_ratio <- function(data) {
  k = 1
  m = 80
  
  prior_mean <- data$alpha / data$beta
  
  exposure_scaling <- 1 + data$N / (m + data$beta)
  s <- k / (prior_mean * exposure_scaling)
  
  data$alpha <- s * prior_mean
  data$beta <- s
  posterior <- list(alpha = data$alpha + data$y, 
                    beta = data$beta + data$N)
  posterior_mean <- posterior$alpha / posterior$beta
  posterior_mean / prior_mean
}

get_paired_scalers_ratio <- function(data, m) {
  k = 1
  
  prior_mean <- data$alpha / data$beta
  rate_scaling <- k / prior_mean
  
  exposure_scaling <- (m + data$beta) / (m + data$beta + data$N)
  s <- rate_scaling * exposure_scaling
  
  data$alpha <- s * prior_mean
  data$beta <- s
  posterior <- list(alpha = data$alpha + data$y, 
                    beta = data$beta + data$N)
  posterior_mean <- posterior$alpha / posterior$beta
  posterior_mean / prior_mean
}


get_min_exposure_scalers_ratio <- function(data) {
  k = 1
  m = 0
  
  prior_mean <- data$alpha / data$beta
  rate_scaling <- k / prior_mean
  
  min_exposure <- min(c(data$beta, data$N))
  exposure_scaling <- min_exposure / (m + data$beta + data$N)
  s <- rate_scaling * exposure_scaling
  
  data$alpha <- s * prior_mean
  data$beta <- s
  posterior <- list(alpha = data$alpha + data$y, 
                    beta = data$beta + data$N)
  posterior_mean <- posterior$alpha / posterior$beta
  posterior_mean / prior_mean
}

data <- list(y = 2, N = 10, alpha = 10, beta = 100)
data <- list(y = 2, N = 10, alpha = 100, beta = 1000)

data <- list(y = 0, N = 1, alpha = 10, beta = 1000)
data <- list(y = 0, N = 1, alpha = 500, beta = 1000)

# these are still in the wrong order even in the version we went with
data <- list(y = 0, N = 2, alpha = 4, beta = 15)
data <- list(y = 0, N = 2, alpha = 40, beta = 150)

data <- list(y = 1, N = 1, alpha = 2, beta = 30) # lower rate
data <- list(y = 1, N = 1, alpha = 6, beta = 714) # should be higher rate

data <- list(y = 1, N = 1, alpha = 18, beta = 656)

data <- list(y = 0, N = 1, alpha = 1, beta = 1)
data <- list(y = 1, N = 1, alpha = 1, beta = 2)
get_paired_scalers_ratio(data, 0)
# get_s_k_m_ratio(data)


get_s_k_ratio(data) # huh, this is the same for our first pair of datas.
get_s_ratio(data)
get_basic_no_hyperparameters_ratio(data)

data$alpha / data$beta
