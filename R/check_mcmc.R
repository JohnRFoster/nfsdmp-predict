#---------
#
# Workflow for checking property-level Bayes model fits of the MIS data
# - combines mcmc chunks
# - checks for convergence
#
#---------

library(dplyr)
library(tidyr)
library(readr)
library(parallel)
library(coda)
library(ggplot2)
library(boaR)

set_boaR_options(pbStyle = as.numeric(Sys.getenv("pbStyle")))
data_store <- Sys.getenv("data_store")

project <- "states"
pull_date <- "2026-03-25"

write_dir <- file.path("out", project)
project_pull <- paste0(project, "-", pull_date)

st <- Sys.getenv("STATENAME")
st <- if_else(st == "", "FLORIDA", st) # for testing
message("\n")
message("STATENAME: ", st)

path <- file.path(write_dir, project_pull, st)

# raw mcmc chunks stored here
mcmc_dir <- file.path(path, "mcmc")

# collated posterior diagnostics go here
analysis_dir <- file.path(path, "analysis")

if (!dir.exists(analysis_dir)) {
  dir.create(analysis_dir, recursive = TRUE)
}

params_check <- c(
  "beta_p",
  "beta1",
  "log_gamma",
  "log_rho",
  "phi_mu",
  "psi_phi",
  "log_nu",
  "p_mu"
)

# number of days in primary period
config_name <- "prod"
config <- config::get(config = config_name)
interval <- config$interval

# whether to create a new dataset of primary periods
# (if FALSE, will use existing dataset)
create_new <- config$create_new

# processed MIS data lives here organized by pull date
mis <- "MIS"
mis_processed <- "processed"
file_name <- "dev_MIS.Effort.Take.all_methods.Daily.Events.csv"

fname <- file.path(data_store, mis, pull_date, mis_processed, file_name)
df <- readr::read_csv(fname, show_col_types = FALSE)

data_mis <- get_data(df, interval, create_new)

data_complete <- data_mis |>
  filter(!is.na(c_road_den), !is.na(c_rugged), !is.na(c_canopy))

jobs <- sort(unique(data_complete$st_name))

# processed MIS data lives here organized by pull date
mis <- "MIS"
mis_processed <- "processed"
file_name <- "dev_MIS.Effort.Take.all_methods.Daily.Events.csv"
pull_date <- "2026-03-25"

config_name <- "prod"
config <- config::get(config = config_name)
interval <- config$interval
create_new <- config$create_new

data_for_nimble <- data_complete |>
  filter(st_name == st) |>
  select(-p) |>
  mutate(primary_period = primary_period - min(primary_period) + 1)

mcmc_diagnostics(
  mcmc_dir = mcmc_dir,
  dest = analysis_dir,
  data = data_for_nimble,
  params_check = params_check
)
