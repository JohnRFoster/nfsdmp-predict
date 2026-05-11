#---------
#
# Workflow for fitting property-level Bayes model to MIS data
#
#---------

library(tidyr)
library(dplyr)
library(readr)
library(nimble)
library(coda)
library(lubridate)
library(boaR)

readRenviron(".env")
data_store <- Sys.getenv("dataPath")

n_chains <- 7
n_iter <- 10000

data_for_nimble <- read_csv(file.path(data_store, "masked_mis_data.csv")) |>
  mutate(property = propertyID, county = county_code)

constants <- nimble_constants(
  df = data_for_nimble,
  interval = 28,
  post_round = "first"
)

create_surv_prior <- function(interval = 4) {
  require(lubridate)
  require(readr)
  require(dplyr)
  require(tidyr)

  data <- read_csv(
    file.path(file.path(data_store, "Vital_Rate_Data.csv")),
    show_col_types = FALSE
  )

  data_usa <- data |>
    filter(
      country == "USA",
      time.period.end != "null",
      time.period.start != "null",
      !paper.ID %in% c(128, 1007, 130, 136)
    ) |> # these papers don't have specified date ranges or are meta-analysis
    mutate(
      time.period.end = mdy(time.period.end),
      time.period.start = mdy(time.period.start)
    )

  surv_data <- data_usa |>
    filter(!is.na(survival.prop)) |>
    select(
      unique.ID,
      paper.ID,
      N.hogs.in.study,
      contains("survival"),
      contains("hunting"),
      state,
      contains("time"),
      method.for.data
    )

  surv_mu <- surv_data |>
    mutate(
      weeks = as.numeric(time.period.end - time.period.start) / 7,
      weeks4 = weeks / interval,
      survival.per.4week = survival.prop^(1 / weeks4),
      logit.survival.per.4week = boot::logit(survival.per.4week)
    ) |>
    filter(survival.per.4week > 0) |>
    mutate(scale_factor = survival.per.4week / survival.prop)

  surv_mu_summary <- surv_mu |>
    summarise(
      mu = mean(survival.per.4week),
      mu.logit = mean(logit.survival.per.4week)
    )

  surv_var <- surv_data |>
    filter(survival.var.type %in% c("SD", "95% CI"))

  surv_sd <- surv_var |>
    filter(survival.var.type == "SD") |>
    mutate(sd = as.numeric(survival.var))

  surv_sd_calc <- surv_var |>
    filter(survival.var.type == "95% CI") |>
    mutate(
      low.CI = as.numeric(stringr::str_extract(
        survival.var,
        "[[:graph:]]*(?=\\-)"
      )),
      high.CI = as.numeric(stringr::str_extract(
        survival.var,
        "(?<=\\-)[[:graph:]]*"
      )),
      sd_low = (low.CI - survival.prop) / -1.96,
      sd_high = (high.CI - survival.prop) / 1.96
    ) |>
    group_by(unique.ID) |>
    summarise(sd = max(sd_high, sd_low))

  surv_var_join <- left_join(surv_var, surv_sd_calc, by = join_by(unique.ID)) |>
    filter(survival.var.type != "SD")

  scale_ids <- surv_mu |>
    select(unique.ID, scale_factor)

  surv_variance <- bind_rows(surv_var_join, surv_sd) |>
    left_join(scale_ids, by = join_by(unique.ID)) |>
    mutate(
      variance = sd^2,
      variance.4week = variance * scale_factor^2,
      sd.4week = sqrt(variance.4week)
    )

  surv_sd_summary <- surv_variance |>
    pull(sd.4week) |>
    mean()

  mu <- surv_mu_summary$mu
  psi <- 1 / mean(surv_variance$variance.4week)
  alpha <- mu * psi
  beta <- (1 - mu) * psi

  return(list(
    alpha = alpha,
    beta = beta
  ))
}

survival_prior <- create_surv_prior(4)

constants$log_rho_mu = rep(0, 5)
constants$log_rho_tau = c(2, 1, 1, 3, 3)
constants$p_mu_mu = rep(0, 2)
constants$p_mu_tau = rep(1, 2)
constants$log_gamma_mu = rep(0, 2)
constants$log_gamma_tau = rep(3, 2)
constants$beta1_mu = rep(0, 5)
constants$beta1_tau = rep(1, 5)
constants$beta_p_mu = rep(0, 15)
constants$beta_p_tau = rep(1, 15)
constants$phi_mu_a = survival_prior$alpha
constants$phi_mu_b = survival_prior$beta
constants$psi_shape = 1
constants$psi_rate = 0.1
constants$log_nu_mu = 2
constants$log_nu_tau = 1

data <- nimble_data(data_for_nimble)

inits <- list(n_chains)
for (i in seq_len(n_chains)) {
  set.seed(i)
  inits[[i]] <- nimble_inits(constants, data, buffer = 200)
}

write_dir <- file.path("out/MMRM", Sys.Date())

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
