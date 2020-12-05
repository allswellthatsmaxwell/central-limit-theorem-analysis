
#' Makes a shape along the range FROM to TO, that's 0 everywhere
#' except for between start and stop. Uses shape_fn to draw.
make_shape <- function(steps, start, stop, shape_fn) {
  sapply(steps, function(x) {
    if (dplyr::between(x, start, stop)) shape_fn(x) else 0
  })
}

normalize <- function(x) x / sum(x)

shift <- function(x, n) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

iterate_convolutions <- function(times, steps, f, g, name, shift_mean = TRUE) {
  times <- times - 1
  fg_df <- convolve_and_entibble(steps, f, g, name, shift_mean)
  fg_temp <- fg_df
  name_orig <- "f * g"
  name_temp <- name_orig
  l <- lapply(1:times, function(i) {
    fg_temp <<- convolve_and_entibble(
      steps, f = f, g = fg_temp %>% dplyr::filter(side == '=') %$% y, 
      name = name)
    fg_temp
  })
  lnames <- lapply(1:times, function(i) {
    name_temp <<- glue::glue("f * {name_temp}")
    name_temp
  })
  l <- append(list(fg_df), l)
  names(l) <- append(list(name_orig), lnames)
  l
}

make_iterated_plots <- function(iterated_dfs, plot_additions = list()) {
  iterated_plots <- iterated_dfs %>%
    lapply(plot_convolutions)
  iterated_plots <- Map(function(p, name) p + ggtitle(name) + plot_additions, 
                        iterated_plots, names(iterated_plots))
  iterated_plots %>% 
  {Reduce(`+`, .)} + plot_layout(nrow = 1)
}


plot_convolutions <- function(dat) {
  dat %>%
    ggplot(aes(x = x, y = y, group = side, color = side)) +
    facet_wrap(~side, ncol = 1) +
    scale_color_manual(values = c(' ' = 'blue', '*' = 'red', '=' = 'black')) +
    geom_line(size = 1.4) +
    theme_bw() +
    scale_x_continuous(breaks = 0, labels = "0") +
    theme(panel.grid = element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 20, vjust = 0.5),
          strip.text = element_text(size = 28, vjust = 0.5),
          strip.background = element_blank())
}

shift_convolution2 <- function(conv, steps, f, g) {
  current_mean <- sum(steps * conv)
  target_mean <- sum(steps * f) + sum(steps * g)
  conv / (current_mean / target_mean)
}

shift_convolution <- function(conv, steps, f, g) {
  step_size <- steps[2] - steps[1]
  current_mean <- sum(steps * conv)
  target_mean <- sum(steps * f) + sum(steps * g)
  distance <- target_mean - current_mean
  steps_in_distance <- round(distance / step_size)
  conv <- shift(conv, steps_in_distance) %>% normalize()
  rnd <- purrr::partial(round, digits = 3)
  conv_mean <- sum(steps * conv)
  print(glue::glue("attempted to move from mean {rnd(current_mean)} to {rnd(target_mean)} \
(a distance of {rnd(distance)}, with {steps_in_distance} steps); \
achieved {rnd(conv_mean)}."))
  conv
}

TYPE <- "circular"
#' Puts f, g, and convolve(f, g) into a long tibble.
convolve_and_entibble <- function(steps, f, g, name, shift_mean = TRUE) {
  ft <- tibble::tibble(name, side = ' ', x = steps, y = f)
  gt <- tibble::tibble(name, side = '*', x = steps, y = g)
  conv <- convolve(f, g, type = TYPE, conj = FALSE)
  if (shift_mean) {
    conv %<>% shift_convolution(steps, f, g)
  }
  print(glue::glue("{sum(steps * f)} + {sum(steps * g)} = {sum(steps * conv)}"))
  ct <- tibble::tibble(name, side = '=', x = steps, y = conv)
  dplyr::bind_rows(ft, gt, ct)
}



convolve_n_times <- function(fdict, n) {
  xs <- fdict[["xs"]]
  f <- fdict[["d"]](xs) %>% normalize()
  g <- f
  dfs <- vector(mode = "list", length = n + 1)
  dfs[[1]] <- tibble::tibble(xs = xs, h = g, convolutions = 0)
  if (n > 1) {
    for (i in seq(from = 2, to = n + 1, by = 1)) {
      g_prev <- g
      g <- convolve(g, f, conj = FALSE, type = "circular") %>% normalize() ## ?
      dfs[[i]] <- tibble::tibble(xs = xs, h = g, convolutions = i - 1)
    }
  }
  dfs
}

attach_ideal_gaussian <- function(df, sds = 10) {
  xs <- df$xs
  h <- df$h
  convolutions <- unique(df$convolutions)
  assertthat::are_equal(length(convolutions), 1)
  hmean <- sum(xs * h)
  hsd <- sqrt(sum(((xs - hmean)^2) * h))
  clipped_inds <- (xs > hmean - hsd * sds) & (xs < hmean + hsd * sds)
  xs <- xs[clipped_inds]
  h <- h[clipped_inds]
  ideal_gaussian <- dnorm(xs, hmean, hsd) %>% normalize()
  tibble::tibble(x = xs, h, ideal_gaussian, convolutions) 
}

get_tail_area <- function(h1, gs) {
  sum(gs > h1) / length(gs)
}

get_probability_greater <- function(dat) {
  h <- dat %$% h
  gauss <- dat %$% ideal_gaussian
  xs <- dat %$% xs
  length_h <- length(h)
  total <- 0
  while(length(h) > 0) {
    h1 <- h[1]
    matches <- h == h1
    count_h <- sum(matches)
    ph <- count_h / length_h
    g_tail_area <- get_tail_area(h1, gauss)
    total <- total + ph * g_tail_area
    h <- h[!matches]
  }
  total
}

make_plot_to_animate <- function(dat, size) {
  dat %>% 
    ggplot(aes(x = x, group = convolutions)) +
      geom_line(aes(y = ideal_gaussian), color = 'black', alpha = 1, size = size) +
      geom_line(aes(y = h), color = '#F8766D', alpha = 0.95, size = size) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 20),
            plot.title = element_text(size = 24),
            axis.title = element_blank(),
            axis.ticks = element_blank()) +
      labs(title = "convolutions: {closest_state}") +
      gganimate::transition_states(convolutions, wrap = FALSE,
                                   transition_length = 0.001,
                                   state_length = 0.001) +
      gganimate::ease_aes('linear') +
      view_follow(fixed_x = FALSE,
                  fixed_y = FALSE)
}

apply_convs_and_stack <- function(fdict, nconvs = 30, sds = 4) {
  fdict %>%
    convolve_n_times(nconvs) %>%
    lapply(function(df) attach_ideal_gaussian(df, sds = sds)) %>%
    dplyr::bind_rows()
}

apply_convs_then_plot <- function(fdict, nconvs = 30, size = 7, sds = 4) {
  fdict %>%
    apply_convs_and_stack(nconvs, sds) %>%
    make_plot_to_animate(size)
}


post_shaper <- function(x) 4


get_moments <- function(fdict) {
  dx <- fdict %$% {xs[2] - xs[1]}
  fdict %>%
    apply_convs_and_stack(sds = 100) %>% 
    group_by(convolutions) %>%
    dplyr::mutate(mass = sum(h),
                  mean = sum(x * h)) %>%
    group_by(convolutions, mass, mean) %>%
    dplyr::summarize(variance = sum((x - mean)^2 * h),
                     skew = sum(((x - mean) / sqrt(variance))^3 * h),
                     kurtosis = sum(((x - mean) / sqrt(variance))^4 * h))
  #s <- wide_sk %>% 
  #  dplyr::mutate(statistic = "skew") %>%
  #  dplyr::select(convolutions, statistic, val = skew)
  #k <- wide_sk %>% 
  #  dplyr::mutate(statistic = "kurtosis") %>%
  #  dplyr::select(convolutions, statistic, val = kurtosis)
  #dplyr::bind_rows(s, k)
}

stack_moments <- function(wide_moments) {
  pick <- function(moment_name) {
    result <- wide_moments %>% 
      dplyr::mutate(moment = moment_name, moment = moment_name) %>%
      .[,c("distribution", "convolutions", "moment", moment_name)]
    colnames(result) <- c("distribution", "convolutions", "moment_name", "moment")
    result
  }
  lapply(c("mass", "mean", "variance", "skew", "kurtosis"), pick) %>%
    Reduce(dplyr::bind_rows, .)
}
