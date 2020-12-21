library(ggplot2)
library(dplyr)
library(magrittr)

normalize <- function(x) x / sum(x)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode <- function(x){
  ta <- table(x)
  tam <- max(ta)
  if (all(ta == tam))
    mod <- NA
  else
    if(is.numeric(x))
      mod <- as.numeric(names(ta)[ta == tam])
  else
    mod <- names(ta)[ta == tam]
  return(mod)
}

mmm_theme <- theme(legend.title = element_blank(), 
                   legend.text = element_text(size = 18),
                   axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank(),
                   axis.text.x = element_text(size = 15),
                   axis.title = element_text(size = 17),
                   legend.position = "top")

dx <- 0.01
xs <- seq(from = 0, to = 20, by = dx)
ys <- dgamma(xs, 2, 1) %>% normalize()
dat <- tibble::tibble(xs, ys)
mode_ <- xs[ys == max(ys)]
mean_ <- sum(xs * ys)
median_ <- qgamma(0.5, 2, 1)

locs_dat <- tibble::tibble(
  parameter = c("Mean", "Median", "Mode"),
  value = c(mean_, median_, mode_))
gamma_locs_plot <- dat %>%
  ggplot(aes(x = xs, y = ys)) +
  geom_line(size = 1.5) +
  geom_vline(data = locs_dat, aes(xintercept = value, color = parameter), size = 1.5) +
  theme_bw() +
  lims(x = c(0, 15)) +
  labs(x = "y", y = "probability density") +
  mmm_theme
  
gamma_locs_plot

ggsave(gamma_locs_plot, path = '../plots', filename = 'gammalocs.png',
       dpi = 100, width = 9.58, height = 3.57, units = "in")

get_mm <- function(vec, name = "?") {
  tibble::tibble(
    name = name,
    parameter = c("Mean", "Median"),
    value = c(mean(vec), median(vec)))
}
set.seed(5)
rs <- rgamma(1000, 2, 1)
rdat_summary <- get_mm(rs)
rdat <- tibble::tibble(xs = 1:1000, rs)
bins <- 60

## Ugh... we should tabulate the histograms first then do our operations
## on that, instead of operating on the data directly like this.
mod1 <- rs
median_loc <- median(rs)
median_val <- rs[rs == min(rs - median_loc)]
mod1[rs >= 2*median_val] %<>% {. + 1}
mod1[(rs > median_val) & (rs < 2 * median_val)] %<>% {. - 1}

summaries <- dplyr::bind_rows(get_mm(mod1, "mod1"), get_mm(rs, "baseline"))
data <- dplyr::bind_rows(tibble::tibble(name = "baseline", val = rs), 
                         tibble::tibble(name = "mod1", val = mod1))

data %>%
  ggplot(aes(x = val)) +
  geom_histogram(bins = 60) +
  geom_vline(data = summaries, aes(xintercept = value, color = parameter), 
             size = 1.5) +
  theme_bw() +
  mmm_theme +
  facet_wrap(~name, ncol = 1)

make_move_plot <- function(vec) {
  tibble::tibble(vec) %>%
    ggplot(aes(x = vec)) +
      geom_histogram(bins = 60) +
      geom_vline(data = get_mm(vec), aes(xintercept = value, color = parameter), 
                 size = 1.5) +
      theme_bw() +
      mmm_theme
}


make_move_plot(rs) + make_move_plot(mod1) + patchwork::plot_layout(ncol = 1)  

