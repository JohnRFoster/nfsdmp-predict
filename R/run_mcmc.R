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
library(parallel)

readRenviron(".env")
set_boaR_options(pbStyle = as.numeric(Sys.getenv("pbStyle")))
data_store <- Sys.getenv("dataPath")

n_chains <- 7
n_iter <- 50000

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
    file.path(data_store, "Vital_Rate_Data.csv"),
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
  inits[[i]]$beta1 <- jitter(c(-1, -3.75, -0.25, 0.3, -1.5))
  inits[[i]]$beta_p <- matrix(
    jitter(
      c(1.75, 1.5, 0, 0, -0.5, -1.2, 0.15, 0, 0.1, -1.75, -1, 0, -0.75, 0, 0)
    ),
    5,
    3
  )
  inits[[i]]$p_mu <- jitter(c(-4, -3))
  inits[[i]]$log_gamma <- jitter(c(-3, -2.1))
  inits[[i]]$log_rho <- jitter(c(0.8, 2.25, 2.15, -1.35, -0.55))
  inits[[i]]$psi_phi <- runif(1, 0.65, 0.7)
  inits[[i]]$phi_mu <- runif(1, 0.57, 0.59)
}

write_dir <- file.path("out/MMRM", Sys.Date())

modelCode <- nimbleCode({
  # priors
  for (i in 1:n_method) {
    log_rho[i] ~ dnorm(log_rho_mu[i], tau = log_rho_tau[i])
  }

  for (i in 1:2) {
    p_mu[i] ~ dnorm(p_mu_mu[i], tau = p_mu_tau[i])
    logit(p_unique[i]) <- p_mu[i]

    log_gamma[i] ~ dnorm(log_gamma_mu[i], tau = log_gamma_tau[i])
  }

  # non time varying coefficients - observation model
  for (i in 1:n_method) {
    beta1[i] ~ dnorm(beta1_mu[i], tau = beta1_tau[i])
  }

  for (i in 1:n_betaP) {
    beta_p[beta_p_row[i], beta_p_col[i]] ~
      dnorm(beta_p_mu[i], tau = beta_p_tau[i])
  }

  # estimate apparent survival
  phi_mu ~ dbeta(phi_mu_a, phi_mu_b)
  psi_phi ~ dgamma(psi_shape, psi_rate)
  a_phi <- phi_mu * psi_phi
  b_phi <- (1 - phi_mu) * psi_phi

  log_nu ~ dnorm(log_nu_mu, tau = log_nu_tau) # mean litter size
  log(nu) <- log_nu

  ## convert to expected number of pigs per primary period
  log_zeta <- log(pp_len) + log_nu - log(365)
  log(zeta) <- log_zeta
  for (i in 1:n_ls) {
    J[i] ~ dpois(nu)
  }

  for (i in 1:n_survey) {
    log_potential_area[i] <- calc_log_area(
      log_rho = log_rho[1:n_method],
      log_gamma = log_gamma[1:2],
      p_unique = p_unique[1:2],
      log_effort_per = log_effort_per[i],
      effort_per = effort_per[i],
      n_trap_m1 = n_trap_m1[i],
      log_pi = log_pi,
      method = method[i]
    )

    # probability of capture, given that an individual is in the surveyed area
    log_theta[i] <- log(
      ilogit(
        beta1[method[i]] +
          inprod(X_p[i, 1:m_p], beta_p[method[i], 1:m_p])
      )
    ) +
      min(0, log_potential_area[i] - log_survey_area_km2[i])

    # likelihood
    y[i] ~ dpois(p[i] * (N[nH_p[i]] - y_sum[i]))
  }

  # the probability an individual is captured on the first survey
  for (i in 1:n_first_survey) {
    log(p[first_survey[i]]) <- log_theta[first_survey[i]]
  }

  # the probability an individual is captured after the first survey
  for (i in 1:n_not_first_survey) {
    log(p[not_first_survey[i]]) <- log_theta[not_first_survey[i]] +
      sum(log(
        1 - exp(log_theta[start[not_first_survey[i]]:end[not_first_survey[i]]])
      ))
  }

  for (i in 1:n_property) {
    lambda_1[i] ~ dunif(n1_min[i], n1_max[i])
    N[nH[i, 1]] ~ dpois(round(lambda_1[i]))

    # population growth across time steps
    for (j in 2:n_time_prop[i]) {
      # loop through every PP, including missing ones

      lambda[nH[i, j - 1]] <- (N[nH[i, j - 1]] - rem[i, j - 1]) *
        zeta /
        2 +
        (N[nH[i, j - 1]] - rem[i, j - 1]) * phi[nH[i, j - 1]]

      N[nH[i, j]] ~ dpois(lambda[nH[i, j - 1]])
      phi[nH[i, j - 1]] ~ dbeta(a_phi, b_phi)
    }
  }
})

cl <- makeCluster(n_chains)
mcmc_parallel(
  cl = cl,
  model_code = modelCode,
  model_constants = constants,
  model_data = data,
  model_inits = inits,
  params_check = c(
    "beta_p",
    "beta1",
    "log_gamma",
    "log_rho",
    "phi_mu",
    "psi_phi",
    "log_nu",
    "p_mu"
  ),
  n_iters = n_iter,
  dest = write_dir,
  monitors_add = "N",
  custom_samplers = NULL,
  export = "calc_log_area"
)
