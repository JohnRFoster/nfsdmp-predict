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
data_store <- Sys.getenv("dataPath")

rds <- read_rds("data/dev_data.rds")

constants <- rds$constants
data <- rds$data
inits <- rds$inits

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
