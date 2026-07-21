library(tidyr)
library(dplyr)
library(readr)
library(nimble)
library(coda)
library(lubridate)
library(boaR)
library(parallel)

set_boaR_options(pbStyle = as.numeric(Sys.getenv("pbStyle")))
data_store <- Sys.getenv("data_store")

project <- "states"

write_dir <- file.path("out", project, Sys.Date())

n_chains <- 3
pull_date <- "2026-03-25"
post_round <- "first"

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

jobs <- unique(data_complete$st_name)
# length(jobs) = 23

# get the array number from slurm
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

st <- jobs[task_id]

data_for_nimble <- data_complete |> filter(st_name == st)

path <- file.path("data", project, paste0("run-date-", Sys.Date()))

if (!dir.exists(path)) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

fname <- paste0("data_for_nimble-", pull_date, "-", st, ".csv")
write_csv(data_for_nimble, fname)

n <- nrow(data_for_nimble)
np <- length(unique(data_for_nimble$propertyID))
nm <- length(unique(data_for_nimble$method))

message("State: ", st)
message("n events: ", n)
message("n properties: ", np)
message("n methods: ", nm)

constants <- nimble_constants(
  df = data_for_nimble,
  interval = 28,
  post_round = "first"
)
data <- nimble_data(data_for_nimble)

# these booleans need to be defined to build the correct model
model_flags <- get_model_flags(data_for_nimble)
params_check <- config$params_check

mcmc_parallel(
  n_chains = n_chains,
  model_constants = constants,
  model_data = data,
  model_flags = model_flags,
  params_check = params_check,
  n_iters = n_iter,
  dest = write_dir,
  monitors_add = "N",
  custom_samplers = NULL,
  export = "calc_log_area"
)


