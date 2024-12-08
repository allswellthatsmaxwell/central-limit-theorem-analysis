library(ggplot2)
library(ggExtra)
library(copula)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)

# Function to create PDF from samples using kernel density estimation
samples_to_pdf <- function(samples, n_points = 1000) {
  # Compute kernel density estimation
  density_obj <- density(samples)
  # Return function that interpolates the PDF
  return(list(
    x = density_obj$x,
    y = density_obj$y,
    pdf = approxfun(density_obj$x, density_obj$y, rule = 2)
  ))
}

# Function to convolve two PDFs
convolve_pdfs <- function(pdf1, pdf2, n_points = 1000) {
  # Create evaluation points
  x_min <- min(pdf1$x) + min(pdf2$x)
  x_max <- max(pdf1$x) + max(pdf2$x)
  x_grid <- seq(x_min, x_max, length.out = n_points)
  
  # Compute convolution integral for each point
  conv_y <- sapply(x_grid, function(z) {
    # For each z, compute integral of f(x)g(z-x)
    integrand <- function(x) {
      pdf1$pdf(x) * pdf2$pdf(z - x)
    }
    integrate(integrand, 
              lower = max(min(pdf1$x), z - max(pdf2$x)),
              upper = min(max(pdf1$x), z - min(pdf2$x)),
              subdivisions = 1000)$value
  })
  
  return(list(
    x = x_grid,
    y = conv_y,
    pdf = approxfun(x_grid, conv_y, rule = 2)
  ))
}

mean_pdf <- function(pdf) {
  integrate(function(x) x * pdf$pdf(x), 
            min(pdf$x), 
            max(pdf$x))$value
}

variance_pdf <- function(pdf) {
  mu <- mean_pdf(pdf)
  # Integrate (x-μ)²*f(x)
  integrate(function(x) (x - mu)^2 * pdf$pdf(x), 
            min(pdf$x), 
            max(pdf$x))$value
}

get_item_names <- function(nitems) {
  item_names <- c("chips", "salsa", "beer", "pop", "water",
                  "tacos", "liquor", "napkins", "plates", "cups")
  additional_names <- sapply(
    (length(item_names) + 1):nitems, function(i) as.character(i))
  item_names %>% c(additional_names)
}

get_copula <- function(nitems) {
  rho_matrix <- runif(nitems*nitems) %>%
    matrix(nitems, nitems) %>%
    Matrix::nearPD() %$%
    mat
  normalCopula(param = rho_matrix[lower.tri(rho_matrix)], 
               dim = nitems, dispstr = "un")
}

get_marginals <- function(nitems) {
  lapply(
    1:nitems, 
    function(i) list(rate = runif(1, 0, 1)))
  # function(i) list(rate = rgamma(1, shape = 0.5, scale = 2)))
}

get_sim_data <- function(item_names) {
  nitems <- length(item_names)
  
  gauss_cop <- get_copula(nitems)
  marginals <- get_marginals(nitems)
  mv_dist <- mvdc(copula = gauss_cop, 
                  margins = rep("exp", nitems), 
                  paramMargins = marginals)
  as.data.frame(rMvdc(1000, mv_dist)) %>% 
    set_colnames(item_names) %>%
    as_tibble()
}

get_comparison_plot <- function(convolution) {
  mu <- mean_pdf(convolution)
  sigma <- sqrt(variance_pdf(convolution))
  lb <- mu - 3 * sigma
  ub <- mu + 3 * sigma
  gaussian_pdf <- dnorm(convolution$x, mu, sigma)
  convolution_dat <- convolution %$% 
    tibble(x=x, y=y, name='convolution')
  comparison_dat <- tibble(x=convolution$x, y=gaussian_pdf, 
                           name='actual gaussian') %>%
    bind_rows(convolution_dat)
  ggplot(comparison_dat, aes(x=x, y=y, color=name)) +
    geom_line() +
    xlim(c(lb, ub)) +
    theme_bw()
}


# Generate your dependent samples as before
nitems <- 80
item_names <- get_item_names(nitems)
sim_data <- get_sim_data(item_names)
cor(sim_data) %>% round(2)

# Convert samples to PDFs
pdfs <- lapply(sim_data, function(x) samples_to_pdf(x))

convolution <- Reduce(convolve_pdfs, tail(pdfs, -1), init=pdfs[[1]])
get_comparison_plot(convolution)
# pdfs[["convolution"]] <- convolution



pdfs_long_dat <- Map(
  function(product, pdf) tibble(product=product, x=pdf$x, y=pdf$y),
  names(pdfs), 
  pdfs) %>% 
  bind_rows()

pdfs_long_dat %>%
  ggplot(aes(x, y, group=product)) +
  geom_line() +
  xlim(-1, 50) +
  theme_bw()

