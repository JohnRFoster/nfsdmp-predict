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

run_date <- "2026-06-22"
project <- "Guam"

# raw mcmc chunks stored here
mcmc_dir <- file.path("out", project, run_date)

# collated posterior diagnostics go here
analysis_dir <- file.path("analysis", project, run_date)

read_path <- file.path(wd, mcmc_dir)
write_path <- file.path(wd, analysis_dir)

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

# processed MIS data lives here organized by pull date
mis <- "MIS"
mis_processed <- "processed"
file_name <- "dev_MIS.Effort.Take.all_methods.Daily.Events.csv"
pull_date <- "2026-03-25"

config_name <- "prod"
config <- config::get(config = config_name)
interval <- config$interval
create_new <- config$create_new

fname <- file.path(data_store, mis, pull_date, mis_processed, file_name)
data_mis <- get_data(fname, interval, create_new)

data_for_nimble <- data_mis |>
	filter(st_name == "GUAM") |>
	mutate(
		across(starts_with("c_"), ~0)
	)

mcmc_diagnostics(
	mcmc_dir = read_path,
	dest = write_path,
	data = data_for_nimble,
	params_check = params_check
)
