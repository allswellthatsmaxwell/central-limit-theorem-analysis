library(ggplot2)
library(ggExtra)
library(copula)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)


item_names <- c("chips", "salsa", "beer", "pop", "water",
                "tacos", "liquor", "napkins", "plates", "cups")
nitems <- length(item_names)
rho_matrix <- runif(nitems*nitems) %>%
  matrix(nitems, nitems) %>%
  Matrix::nearPD() %$%
  mat
gauss_cop <- normalCopula(param = rho_matrix[lower.tri(rho_matrix)], 
                          dim = nitems, dispstr = "un")



# Step 2: Define Marginal Distributions
marginals <- lapply(
  1:nitems, 
  function(i) list(rate = rgamma(1, shape = 0.5, scale = 2)))

# Step 3: Create the Multivariate Distribution
# Combine the Gaussian copula with the marginals
mv_dist <- mvdc(copula = gauss_cop, 
                margins = rep("exp", nitems), 
                paramMargins = marginals)


# Step 4: Simulate Data
sim_data <- as.data.frame(rMvdc(1000, mv_dist)) %>% 
  set_colnames(item_names) %>%
  as_tibble()


long_data <- sim_data %>%
  pivot_longer(cols = everything(), 
             names_to = "item", 
             values_to = "purchases") %>%
  arrange(item)

long_data %>%
  filter(purchases <= 20) %>%
  ggplot(aes(x = purchases, group=item)) +
  geom_density() +
  #facet_wrap(~item, ncol=1) +
  theme_minimal()

cor(sim_data) %>% round(2)

display_rhos <- rho_matrix %>% 
  set_rownames(item_names) %>% 
  set_colnames(item_names) %>%
  round(2)

vectors <- lapply(sim_data, identity)

convolution <- Reduce(convolve, 
                      x = tail(vectors, nitems - 1), 
                      init = vectors[[1]])
convolution <- sim_data %$% convolve(beer, water)
convolution <- sim_data %$% beer
# huh, the mean is way too big. This has to do with
# sampling vs. pdfs, I think. So should we convert to pdfs
# before convolution?
convolution %>%
  tibble(x = .) %>%
  ggplot(aes(x=x)) +
  geom_density() +
  theme_minimal()
