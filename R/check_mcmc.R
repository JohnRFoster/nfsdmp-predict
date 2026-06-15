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
fs_path <- Sys.getenv("fs_path")
project_path <- Sys.getenv("project_path")
data_store <- Sys.getenv("data_store")
wd <- file.path(fs_path, project_path)

run_date <- "2026-06-12"
mcmc_dir <- file.path("out/MMRM", run_date)

read_path <- file.path(wd, mcmc_dir)
write_path <- file.path(wd, "analysis/MMRM", run_date)

if (!dir.exists(write_path)) {
	dir.create(write_path, recursive = TRUE)
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

data_for_nimble <- read_csv(file.path(data_store, "masked_mis_data.csv")) |>
	mutate(property = propertyID, county = county_code)

mcmc_diagnostics(
	mcmc_dir = read_path,
	dest = write_path,
	data = data_for_nimble,
	params_check = params_check
)
