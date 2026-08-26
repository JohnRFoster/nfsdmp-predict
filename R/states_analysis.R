library(dplyr)
library(readr)
library(ggplot2)


# need to match which params match each method because states use different methods!

w <- 0.5
all_params |>
  filter(grepl("nu", node)) |>
  ggplot() +
  aes(
    x = median,
    xmin = quantile_0.05,
    xmax = quantile_0.95,
    y = node,
    colour = st_name,
    group = st_name
  ) +
  geom_point(position = position_dodge(width = w)) +
  geom_linerange(position = position_dodge(width = w)) +
  theme_bw()
