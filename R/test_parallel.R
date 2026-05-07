# ----------------------------------
# This script tests the parallel MCMC function.
# It should be run on a machine with multiple cores,
# and will save the MCMC output in chunks to the
# directory specified in the config file.
# ----------------------------------

# remotes::install_github("JohnRFoster/boaR")

library(tidyr)
library(dplyr)
library(readr)
library(coda)
library(lubridate)
library(boaR)
library(parallel)
set.seed(131)

# number of iterations and chains for mcmc
# "default" for testing, "prod" for final runs
config_name <- "dev_parallel"
config <- config::get(config = config_name)
n_iter <- config$n_iter
n_chains <- 3

readRenviron(".env")
set_boaR_options(pbStyle = as.numeric(Sys.getenv("pbStyle")))
data_store <- Sys.getenv("dataPath")
pull_date <- "2026-03-25"
post_round <- "last"

# number of days in primary period
interval <- config$interval

# whether to create a new dataset of primary periods
# (if FALSE, will use existing dataset)
create_new <- config$create_new

# processed MIS data lives here organized by pull date
mis <- "MIS"
mis_processed <- "processed"
file_name <- "dev_MIS.Effort.Take.all_methods.Daily.Events.csv"

fname <- file.path(data_store, mis, pull_date, mis_processed, file_name)
data_mis <- get_data(fname, interval, create_new)

fname <- "originalFitDataWithIDs.csv"
original_ids <- read_csv(file.path(data_store, fname))

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
	"OKLAHOMA",
	"MISSOURI"
)

data_new_props_ms <- data_mis |>
	filter(
		propertyID %in% new_not_in_original,
		st_name %in% state_vec,
	)

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
	post_round = post_round
)

data <- nimble_data(data_new_props)

inits <- list(n_chains)
for (i in seq_len(n_chains)) {
	set.seed(i)
	inits[[i]] <- nimble_inits(constants, data, buffer = 200)
}

write_dir <- file.path(config$mcmc_dir, Sys.Date())

cl <- makeCluster(n_chains)
mcmc_parallel(
	cl = cl,
	model_code = nimble_removal_model(),
	model_constants = constants,
	model_data = data,
	model_inits = inits,
	params_check = config$params_check,
	n_iters = n_iter,
	dest = write_dir,
	monitors_add = "N",
	custom_samplers = NULL
)
