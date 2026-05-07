# ----------------------------------
# This script tests the parallel MCMC function.
# It should be run on a machine with multiple cores,
# and will save the MCMC output in chunks to the
# directory specified in the config file.
# ----------------------------------

library(tidyr)
library(dplyr)
library(readr)
library(nimble)
library(coda)
library(lubridate)
library(boaR)


config_name <- "dev_parallel"
config <- config::get(config = config_name)
n_iter <- config$n_iter
n_chains <- 3

readRenviron(".env")
data_store <- Sys.getenv("dataPath")
