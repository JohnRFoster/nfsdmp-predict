# ----------------------------------
# This script tests the particle filter workflow.
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
	# "OKLAHOMA",
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

rmodel <- nimble::nimbleModel(
	code = nimble_removal_model(),
	constants = constants,
	data = data,
	inits = nimble_inits(constants, data),
	calculate = TRUE
)

aux_filter <- nimbleSMC::buildAuxiliaryFilter(
	rmodel,
	"N",
	control = list(
		saveAll = TRUE,
		smoothing = FALSE,
		lookahead = "simulate"
	)
)

c_model <- compileNimble(rmodel)
c_aux_filter <- compileNimble(aux_filter)
logLik <- c_aux_filter$run(m = 10000)
ess <- c_aux_filter$returnESS()
aux_n <- as.matrix(c_aux_filter$mvEWSamples, "N")
aux_n[1:10, 1:10]
dim(aux_n)

n_idx <- unique(model_constants$nH_p)
n_nodes <- paste0("N[", n_idx, "]")
n_samples <- aux_n[, n_nodes]
dim(n_samples)
N <- apply(n_samples, 2, median)
summary(N)
quantile(N, c(0.025, 0.5, 0.975))
