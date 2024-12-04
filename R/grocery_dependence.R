library(ggplot2)
library(ggExtra)
library(copula)
library(tidyr)
library(dplyr)
library(tibble)

# Step 1: Define the Gaussian Copula

nitems <- 10
rho_matrix <- runif(nitems*nitems) %>%
  matrix(nitems, nitems) %>%
  Matrix::nearPD() %$%
  mat
gauss_cop <- normalCopula(param = rho_matrix[lower.tri(rho_matrix)], 
                          dim = nitems, dispstr = "un")



# Step 2: Define Marginal Distributions
# Marginals for chips and dip sales (exponential distributions)
marginals <- lapply(1:nitems, function(i) list(rate = runif(1)))
# marginals <- list(list(rate = 0.5), list(rate = 0.3))

# Step 3: Create the Multivariate Distribution
# Combine the Gaussian copula with the marginals
mv_dist <- mvdc(copula = gauss_cop, 
                margins = rep("exp", nitems), 
                paramMargins = marginals)


# Step 4: Simulate Data
sim_data <- as.data.frame(rMvdc(1000, mv_dist)) %>% 
  set_colnames(c("chips", "salsa", "beer", "pop", "water",
                 "tacos", "liquor", "napkins", "plates", "cups")) %>%
  as_tibble()

cor(sim_data) %>% round(2)

p <- ggplot(sim_data, aes(x = chips, y = dip)) +
  geom_point(alpha = 0.6) +
  labs(title = "Simulated Sales: Chips vs. Dip (Correlated Data)",
       x = "Chips Sales", 
       y = "Dip Sales") +
  theme_minimal()
ggMarginal(p, type="histogram")


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
