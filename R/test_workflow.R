# workflow to fit new properties

library(tidyr)
library(dplyr)
library(readr)
library(nimble)
library(coda)
library(lubridate)
library(boaR)
set.seed(131)

# number of iterations and chains for mcmc
# "default" for testing, "prod" for final runs
config_name <- "default"
config <- config::get(config = config_name)
n_iter <- config$n_iter
n_chains <- config$n_chains


readRenviron(".env")
data_store <- Sys.getenv("dataPath")

# injest data ----
insitu <- "insitu"
pull_date <- "2026-03-25"
post_round <- "first"

# number of days in primary period
interval <- 28

# whether to create a new dataset of primary periods
# (if FALSE, will use existing dataset)
create_new <- FALSE

file_name <- "dev_MIS.Effort.Take.all_methods.Daily.Events.csv"

fname <- file.path(data_store, insitu, pull_date, file_name)
data_mis <- get_data(fname, interval, create_new, data_store)

original_ids <- read_csv("../data-store/originalFitDataWithIDs.csv")

original_props <- unique(original_ids$propertyID)
new_props <- unique(data_mis$propertyID)

# properties in original data not in new data
original_not_in_new <- setdiff(original_props, new_props)
length(original_not_in_new)

# properties in new data not in original data
new_not_in_original <- setdiff(new_props, original_props)
length(new_not_in_original)

# testing on new properties in texas ----
state_vec <- c(
  "TEXAS",
  #"OKLAHOMA",
  "MISSOURI"
)

data_new_props_ms <- data_mis |>
  filter(
    propertyID %in% new_not_in_original,
    st_name %in% state_vec,
  )
# new_props_fw <- data_mis |>
#   filter(
#     propertyID %in% new_not_in_original,
#     st_name == "TEXAS",
#     method == "FIXED WING"
#   ) |>
#   pull(propertyID)

# data_new_props_fw <- data_mis |>
#   filter(
#     propertyID %in% new_props_fw
#   )
data_new_props <- data_new_props_ms |>
  mutate(primary_period = primary_period - min(primary_period) + 1) |>
  arrange(propertyID, primary_period) |>
  mutate(rowID = row_number()) |>
  group_by(propertyID, primary_period) |>
  mutate(N_order = cur_group_id()) |>
  ungroup()
glimpse(data_new_props)

id_lut <- data_new_props |>
  select(ppID, N_order) |>
  distinct()

length(unique(data_new_props$method))

# prep for mcmc ----
# prep nimble ----
constants <- nimble_constants(
  df = data_new_props,
  interval = interval,
  data_repo = data_store,
  post_round = post_round
)

data <- nimble_data(data_new_props)

inits <- list(n_chains)
for (i in seq_len(n_chains)) {
  set.seed(i)
  inits[[i]] <- nimble_inits(constants, data, buffer = 200)
}

samples <- single_mcmc_chain(
  model_constants = constants,
  model_data = data,
  model_code = nimble_removal_model(),
  init = inits[[1]],
  n_iter = n_iter,
  monitors_add = "N"
)

n_idx <- unique(constants$nH_p)
n_nodes <- paste0("N[", n_idx, "]")
N_mat <- samples[, n_nodes]
dim(N_mat)
N <- apply(N_mat, 2, median)

data_n <- data_new_props |>
  mutate(nH_p = constants$nH_p) |>
  group_by(propertyID, primary_period, property_area_km2, nH_p) |>
  reframe(take = sum(take)) |>
  arrange(nH_p) |>
  mutate(
    N = N,
    density = N / property_area_km2,
    take_density = take / property_area_km2
  )

testthat::expect_all_true(data_n$N >= data_n$take)

summary(data_n$density)
quantile(data_n$density, c(0.05, 0.5, 0.95))
