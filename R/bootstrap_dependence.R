library(ggplot2)

# Function to perform dependent bootstrap
dependent_bootstrap <- function(data, n_bootstraps = 1000, dependency_factor = 0.5) {
  n <- length(data)
  bootstrap_means <- numeric(n_bootstraps)
  sample_history <- vector(mode = "list", length = n_bootstraps)
  
  # Initial bootstrap sample
  prev_sample <- sample(data, size = n, replace = TRUE)
  sample_history[[1]] <- prev_sample
  # bootstrap_means[1] <- mean(prev_sample)
  
  
  
  for(i in 2:n_bootstraps) {
    # Calculate weights based on previous sample
    # Creates a matrix of distances between each point and previous sample
    distances <- outer(data, prev_sample, function(x, y) abs(x - y))
    weights <- exp(-dependency_factor * apply(distances, 1, min))
    weights <- weights / sum(weights)
    
    # Generate new sample using these weights
    current_sample <- sample(data, size = n, replace = TRUE, prob = weights)
    sample_history[[i]] <- current_sample
    # bootstrap_means[i] <- mean(current_sample)
    prev_sample <- current_sample
  }
  
  sample_history
}

# Generate test data
original_data <- c(rexp(500), rexp(500))


n_bootstraps <- 1000

samples <- dependent_bootstrap(original_data,
                               n_bootstraps = n_bootstraps, 
                               dependency_factor = 0.10)

samples_dat <- Map(
  function(sample, i) {
    tibble(i=i, x=sample)
  }, samples, 1:length(samples)) %>%
  dplyr::bind_rows()

samples_dat %>%
  filter(i <= 10) %>%
  ggplot(aes(x=x, group=i)) +
  # facet_wrap(i, ncol=1) +
  geom_density() +
  theme_minimal()
  

# Run both regular and dependent bootstrap
regular_bootstrap_means <- replicate(n_bootstraps, 
                                     mean(sample(original_data, 
                                                 size = length(original_data), 
                                                 replace = TRUE)))
dependent_means <- dependent_bootstrap(original_data,
                                       n_bootstraps = n_bootstraps, 
                                       dependency_factor = 0.99)

# Create a data frame for plotting
plot_data <- data.frame(
  means = c(regular_bootstrap_means, dependent_means),
  type = rep(c("Regular", "Dependent"), each = 1000)
)

# Create plots
p <- ggplot(plot_data, aes(x = means, color = type)) +
  geom_density(alpha = 0.5) +
  # facet_wrap(~type) +
  theme_minimal() +
  labs(title = "Distribution of Bootstrap Means",
       x = "Sample Mean",
       y = "Density") +
  theme(legend.title = element_blank(), 
        legend.position = "top")

p

# Perform Shapiro-Wilk test for normality
shapiro.test(regular_bootstrap_means)
shapiro.test(dependent_means)

print(p)
