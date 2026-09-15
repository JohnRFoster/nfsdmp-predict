library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

data_store <- "../data-store"
project <- "US-territories"
run_date <- "2026-06-25"
file_density <- "densitySummaries.rds"
data_density <- read_rds(file.path(data_store, project, run_date, file_density))

file_model_data <- "US-territories_for_nimble-2026-06-25.csv"
data_model <- read_csv(file.path("data", file_model_data))

properties <- data_model |>
  group_by(
    propertyID,
    agrp_prp_id,
    alws_agrprop_id,
    property_area_km2,
    start_dates,
    end_dates,
    st_name,
    cnty_name,
    primary_period
  ) |>
  reframe(take = sum(take)) |>
  mutate(take_density = take / property_area_km2) |>
  left_join(data_density)

glimpse(properties)

density <- properties |>
  distinct() |>
  select(
    agrp_prp_id,
    alws_agrprop_id,
    property_area_km2,
    start_dates,
    end_dates,
    st_name,
    cnty_name,
    take_density,
    mean,
    variance,
    `0.5`,
    `0.025`,
    `0.975`
  ) |>
  rename(
    median = `0.5`,
    lower = `0.025`,
    upper = `0.975`
  )

glimpse(density)
length(unique(density$agrp_prp_id))
length(unique(density$alws_agrprop_id))

summary(density$median)

density |>
  filter(st_name == "GUAM") |>
  ggplot() +
  aes(x = end_dates, ymin = lower, y = median, ymax = upper) +
  geom_ribbon(fill = "lightgrey") +
  geom_line() +
  geom_point(aes(y = take_density)) +
  facet_wrap(~agrp_prp_id, scales = "free") +
  theme_bw()

out_dir <- "../data-store/densityEstimates"
fname <- paste0(project, "ByProperty-", Sys.Date(), ".csv")
dest <- file.path(out_dir, fname)
write_csv(density, dest)


file_samples <- "stateSamples.rds"
samples <- read_rds(file.path(data_store, project, run_date, file_samples))

samps_long <- samples |>
  pivot_longer(cols = everything(), names_to = "node", values_to = "abundance")

samps_long <- samples |>
  mutate(iter = 1:n()) |>
  pivot_longer(cols = -iter, names_to = "node", values_to = "abundance")

property_info <- properties |>
  select(
    node,
    propertyID,
    st_name,
    start_dates,
    end_dates,
    take,
    property_area_km2
  )

tmp_join <- left_join(samps_long, property_info)

tmp <- tmp_join |>
  group_by(iter, st_name, start_dates, end_dates) |>
  reframe(
    take = sum(take),
    abundance = sum(abundance),
    area_surveyed = sum(property_area_km2)
  )

head(plyr::count(tmp$iter))
length(unique(property_info$end_dates))

island_wide2 <- tmp |>
  mutate(
    density = abundance / area_surveyed,
    take_density = take / area_surveyed,
    log_density = log(abundance + 1) - log(area_surveyed),
    log_take_density = log(take + 1) - log(area_surveyed),
  ) |>
  group_by(
    st_name,
    start_dates,
    end_dates,
    take_density,
    log_take_density,
    area_surveyed
  ) |>
  reframe(
    mean = mean(density),
    variance = var(density),
    lower = quantile(density, 0.05),
    median = quantile(density, 0.5),
    upper = quantile(density, 0.95),
    log_mean = mean(log_density),
    log_lower = quantile(log_density, 0.05),
    log_median = quantile(log_density, 0.5),
    log_upper = quantile(log_density, 0.95)
  )

island_wide2 |>
  ggplot() +
  aes(x = end_dates, ymin = lower, y = median, ymax = upper) +
  geom_ribbon(fill = "lightgrey") +
  geom_line() +
  geom_point(aes(y = take_density)) +
  facet_wrap(~st_name, scales = "free") +
  theme_bw()

fname <- paste0(project, "IslandWide-", Sys.Date(), ".csv")
dest <- file.path(out_dir, fname)
write_csv(island_wide2, dest)


# 2025

data_model |>
  mutate(year = year(end_dates)) |>
  group_by(year, st_name, method) |>
  reframe(
    take = sum(take),
    total_units = sum(trap_count),
    total_effort = sum(effort),
    effort_per = total_effort / total_units,
    rows = n()
  ) |>
  mutate(big_year = if_else(year %in% c(2025, 2020), 1, 0)) |>
  View()
