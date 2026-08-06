library(tidyr)
library(dplyr)
library(readr)
library(nimble)
library(coda)
library(lubridate)
library(boaR)
library(parallel)

source("R/state_inits.R")

set_boaR_options(pbStyle = as.numeric(Sys.getenv("pbStyle")))
data_store <- Sys.getenv("data_store")

project <- "states"

write_dir <- file.path("out", project)

n_chains <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
message("n_chains: ", n_chains)

pull_date <- "2026-03-25"
post_round <- "first"

project_pull <- paste0(project, "-", pull_date)

config_name <- "prod"
config <- config::get(config = config_name)
n_iter <- config$n_iter

# number of days in primary period
interval <- config$interval

# whether to create a new dataset of primary periods
# (if FALSE, will use existing dataset)
create_new <- config$create_new

# processed MIS data lives here organized by pull date
mis <- "MIS"
mis_processed <- "processed"
file_name <- "dev_MIS.Effort.Take.all_methods.Daily.Events.csv"

## check for guam and pacific islands data in raw data set

fname <- file.path(data_store, mis, pull_date, mis_processed, file_name)
df <- readr::read_csv(fname, show_col_types = FALSE)

data_mis <- get_data(df, interval, create_new)

data_complete <- data_mis |>
  filter(!is.na(c_road_den), !is.na(c_rugged), !is.na(c_canopy))

jobs <- sort(unique(data_complete$st_name))
# length(jobs) = 23

# get the STATENAME from bash script
st <- Sys.getenv("STATENAME")
st <- if_else(st == "", "FLORIDA", st) # for testing
message("\n")
message("STATENAME: ", st)

path <- file.path(write_dir, project_pull, st)

if (!dir.exists(path)) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

data_for_nimble <- data_complete |>
  filter(st_name == st) |>
  select(-p) |>
  mutate(primary_period = primary_period - min(primary_period) + 1)

n <- nrow(data_for_nimble)
np <- length(unique(data_for_nimble$propertyID))
nm <- length(unique(data_for_nimble$method))
method_names <- data_for_nimble$method
methods <- unique(method_names)

message("n events: ", n)
message("n properties: ", np)
message("n methods: ", nm, " (", paste(methods, collapse = ", "), ")")

constants <- nimble_constants(
  df = data_for_nimble,
  interval = 28,
  post_round = "first"
)
data <- nimble_data(data_for_nimble)

# not all states use all methods, so we need to create a lookup table
# for the methods used in each state beacuse the indexes will change
# given the unique methods used in each state
method_lookup_table <- tibble(
  method_names = method_names,
  m_vec = boaR:::method_factors(data_for_nimble),
  ts_id = constants$ts_id
) |>
  mutate(method_idx = as.numeric(as.factor(m_vec))) |>
  distinct() |>
  arrange(method_idx)

write_rds(method_lookup_table, file.path(path, "method_lookup_table.rds"))

# these booleans need to be defined to build the correct model
model_flags <- get_model_flags(data_for_nimble)
params_check <- config$params_check
dest <- file.path(path, "mcmc")

# get inits for each state

init_list <- state_inits(st)

# runs the mcmc and saves chunks of samples
# will run until conveged
mcmc_parallel(
  n_chains = n_chains,
  model_constants = constants,
  model_data = data,
  model_flags = model_flags,
  params_check = params_check,
  n_iters = n_iter,
  dest = dest,
  monitors_add = "N",
  custom_samplers = NULL,
  export = "calc_log_area",
  buffer = 500,
  beta1 = init_list$beta1,
  beta_p = init_list$beta_p,
  p_mu = init_list$p_mu,
  log_gamma = init_list$log_gamma,
  log_rho = init_list$log_rho,
  psi_phi = init_list$psi_phi,
  phi_mu = init_list$phi_mu,
  log_nu = init_list$log_nu
)

# analysis
analysis_dir <- file.path(path, "analysis")
if (!dir.exists(analysis_dir)) {
  dir.create(analysis_dir, showWarnings = FALSE, recursive = TRUE)
}

mcmc_diagnostics(
  mcmc_dir = dest,
  dest = analysis_dir,
  data = data_for_nimble,
  params_check = params_check
)
