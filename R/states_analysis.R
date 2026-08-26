library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)

analysis_dir <- "analysis"
project <- "states"
pull_date <- "2026-03-25"
read_dir <- file.path(analysis_dir, project, paste0(project, "-", pull_date))
df <- read_rds(file.path(read_dir, "all_params.rds"))
df_priors <- read_rds(file.path("data", "priors_df.rds"))

states <- c(sort(unique(df$st_name)), "Prior")

df_params <- bind_rows(
  select(df, -position),
  select(df_priors, -position, -method_idx),
) |>
  mutate(
    group = case_when(
      st_name == "Prior" ~ "Prior",
      .default = "Posterior"
    ),
    st_name = factor(st_name, levels = states),
  )

my_linerange <- function(df, w = 0.5, lw = 1.25) {
  ggplot(df) +
    aes(
      x = median,
      xmin = quantile_0.05,
      xmax = quantile_0.95,
      y = st_name,
      colour = group,
      group = st_name
    ) +
    geom_point(position = position_dodge(width = w), size = 3) +
    geom_linerange(position = position_dodge(width = w), linewidth = lw) +
    scale_color_manual(
      values = c("Prior" = "#7570b3", "Posterior" = "#1b9e77")
    ) +
    theme_bw() +
    labs_pubr()
}

df_params |>
  filter(grepl("Intercept", land_type)) |>
  my_linerange() +
  facet_wrap(~method_names, scales = "free_x") +
  labs(
    x = "Mean removal rate",
    y = "State",
    title = "Mean removal rate by method",
  ) +
  theme(legend.position = "none")

df_params |>
  filter(grepl("Road density", land_type)) |>
  my_linerange() +
  facet_wrap(~method_names) +
  labs(
    x = "Effect",
    y = "State",
    title = "Effect of road density on removal rate",
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

df_params |>
  filter(grepl("Ruggedness", land_type)) |>
  my_linerange() +
  facet_wrap(~method_names) +
  labs(
    x = "Effect",
    y = "State",
    title = "Effect of terrain ruggedness on removal rate",
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

df_params |>
  filter(grepl("Canopy cover", land_type)) |>
  my_linerange() +
  facet_wrap(~method_names) +
  labs(
    x = "Effect",
    y = "State",
    title = "Effect of canopy cover on removal rate",
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none")

df_params |>
  filter(grepl("gamma", node)) |>
  my_linerange() +
  facet_wrap(~method_names, scales = "free_x") +
  labs(
    x = "Effect",
    y = "State",
    title = "Saturation effect of effort on removal rate",
  ) +
  theme(legend.position = "none")

df_params |>
  filter(grepl("p_mu", node)) |>
  my_linerange() +
  facet_wrap(~method_names) +
  labs(
    x = "Effect",
    y = "State",
    title = "Proportion of unique area surveyed by additional units deployed",
  ) +
  theme(legend.position = "none")

df_params |>
  filter(grepl("rho", node)) |>
  my_linerange() +
  facet_wrap(~method_names, scales = "free_x") +
  labs(
    x = "Effect",
    y = "State",
    title = "Scaling effect of effort on removal rate",
  ) +
  theme(legend.position = "none")


df_dem <- df_params |>
  filter(
    node %in% c("phi_mu", "psi_phi", "zeta", "nu", "lambda", "lambda_annual")
  ) |>
  mutate(
    node = case_when(
      node == "phi_mu" ~ "Mean 28-day survival rate",
      node == "psi_phi" ~ "Shrinkage",
      node == "zeta" ~ "28-day per-capita recruitment rate",
      node == "nu" ~ "Litter size",
      node == "lambda" ~ "28-day growth rate",
      node == "lambda_annual" ~ "Annual growth rate",
      .default = node
    )
  )


df_dem |>
  my_linerange() +
  facet_wrap(~node, scales = "free_x") +
  labs(
    x = "Parameter estimate",
    y = "State",
    title = "Demographic parameters by state",
  ) +
  theme(legend.position = "none")


df_density <- read_rds(file.path(read_dir, "all_density.rds"))

summary(df_density$mean)
quantile(df_density$`0.5`, c(0.05, 0.5, 0.95))

df_density |>
  ggplot() +
  aes(
    x = `0.5`,
    y = st_name,
    xmin = `0.05`,
    xmax = `0.95`
  ) +
  geom_point(size = 3) +
  geom_linerange(linewidth = 1.25) +
  theme_bw()
