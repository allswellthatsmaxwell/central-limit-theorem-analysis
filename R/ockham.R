
library(ggplot2)

range <- 4
xs <- seq(from = -range, to = range, by = 0.01)
confident <- dnorm(xs, 0, 0.2)
unsure <- dnorm(xs, 0, 0.8)
idk <- dunif(xs, -range - 1, range + 1)

blank_theme <- theme(
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_text(size = 16)
  #axis.title = element_blank()
)

linesize <- 2
tibble::tibble(xs, confident, unsure) %>%
  ggplot(aes(x = xs)) +
  geom_line(aes(y = idk), size = linesize, color = 'gray') +
  
  #geom_line(aes(y = confident), size = linesize, color = '#FF65AE') +
  #geom_line(aes(y = unsure), size = linesize, color = "lightblue") +
  labs(x = "a", y = "probability mass") +
  theme_bw() +
  blank_theme

ggsave(path = "../plots/", filename = "ignorant.png", width = 7.395, height = 3.260,
       dpi = 120)
