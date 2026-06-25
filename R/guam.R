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

project <- "US-territories"

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

df <- read_csv(fname)
df |>
  filter(st_name == "GUAM") |>
  glimpse() # 18035

data_mis <- get_data(fname, interval, create_new)

territories <- c(
  "AMERICAN SAMOA",
  "GUAM",
  "NORTHERN MARIANA ISLANDS",
  "PUERTO RICO",
  "VIRGIN ISLANDS"
)

data_for_nimble <- data_mis |>
  filter(st_name %in% territories) |>
  mutate(
    across(starts_with("c_"), ~0)
  )

glimpse(data_for_nimble)

fname <- file.path("data", paste0(project, "_for_nimble-", Sys.Date(), ".csv"))
write_csv(data_for_nimble, fname)

length(unique(data_for_nimble$propertyID))

constants <- nimble_constants(
  df = data_for_nimble,
  interval = 28,
  post_round = "first"
)

# these booleans need to be defined to build the correct model
model_flags <- get_model_flags(data_for_nimble)
data <- nimble_data(data_for_nimble)
params_check <- config$params_check

cl <- makeCluster(n_chains, type = config$cluster_type)
mcmc_parallel(
  cl = cl,
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
