library(dplyr)
library(stringr)

method_pattern <- "(?<=\\[)\\d"
position_pattern <- "(?<=\\, )\\d"

hyperparams <- boaR:::hyperparams
n <- 10000

pr2sd <- function(x) {
	1 / sqrt(x)
}

priors <- tibble(
	"beta1[1]" = rnorm(
		n,
		hyperparams$beta1_mu[1],
		pr2sd(hyperparams$beta1_tau[1])
	),
	"beta1[2]" = rnorm(
		n,
		hyperparams$beta1_mu[2],
		pr2sd(hyperparams$beta1_tau[2])
	),
	"beta1[3]" = rnorm(
		n,
		hyperparams$beta1_mu[3],
		pr2sd(hyperparams$beta1_tau[3])
	),
	"beta1[4]" = rnorm(
		n,
		hyperparams$beta1_mu[4],
		pr2sd(hyperparams$beta1_tau[4])
	),
	"beta1[5]" = rnorm(
		n,
		hyperparams$beta1_mu[5],
		pr2sd(hyperparams$beta1_tau[5])
	),
	"beta_p[1, 1]" = rnorm(
		n,
		hyperparams$beta_p_mu[1],
		pr2sd(hyperparams$beta_p_tau[1])
	),
	"beta_p[1, 2]" = rnorm(
		n,
		hyperparams$beta_p_mu[2],
		pr2sd(hyperparams$beta_p_tau[2])
	),
	"beta_p[1, 3]" = rnorm(
		n,
		hyperparams$beta_p_mu[3],
		pr2sd(hyperparams$beta_p_tau[3])
	),
	"beta_p[2, 1]" = rnorm(
		n,
		hyperparams$beta_p_mu[4],
		pr2sd(hyperparams$beta_p_tau[4])
	),
	"beta_p[2, 2]" = rnorm(
		n,
		hyperparams$beta_p_mu[5],
		pr2sd(hyperparams$beta_p_tau[5])
	),
	"beta_p[2, 3]" = rnorm(
		n,
		hyperparams$beta_p_mu[6],
		pr2sd(hyperparams$beta_p_tau[6])
	),
	"beta_p[3, 1]" = rnorm(
		n,
		hyperparams$beta_p_mu[7],
		pr2sd(hyperparams$beta_p_tau[7])
	),
	"beta_p[3, 2]" = rnorm(
		n,
		hyperparams$beta_p_mu[8],
		pr2sd(hyperparams$beta_p_tau[8])
	),
	"beta_p[3, 3]" = rnorm(
		n,
		hyperparams$beta_p_mu[9],
		pr2sd(hyperparams$beta_p_tau[9])
	),
	"beta_p[4, 1]" = rnorm(
		n,
		hyperparams$beta_p_mu[10],
		pr2sd(hyperparams$beta_p_tau[10])
	),
	"beta_p[4, 2]" = rnorm(
		n,
		hyperparams$beta_p_mu[11],
		pr2sd(hyperparams$beta_p_tau[11])
	),
	"beta_p[4, 3]" = rnorm(
		n,
		hyperparams$beta_p_mu[12],
		pr2sd(hyperparams$beta_p_tau[12])
	),
	"beta_p[5, 1]" = rnorm(
		n,
		hyperparams$beta_p_mu[13],
		pr2sd(hyperparams$beta_p_tau[13])
	),
	"beta_p[5, 2]" = rnorm(
		n,
		hyperparams$beta_p_mu[14],
		pr2sd(hyperparams$beta_p_tau[14])
	),
	"beta_p[5, 3]" = rnorm(
		n,
		hyperparams$beta_p_mu[15],
		pr2sd(hyperparams$beta_p_tau[15])
	),
	"nu" = exp(rnorm(n, hyperparams$log_nu_mu, pr2sd(hyperparams$log_nu_tau))),
	"phi_mu" = rbeta(n, hyperparams$phi_mu_a, hyperparams$phi_mu_b),
	"psi_phi" = rgamma(n, hyperparams$psi_shape, hyperparams$psi_rate),
	"rho[1]" = rnorm(
		n,
		hyperparams$log_rho_mu[1],
		pr2sd(hyperparams$log_rho_tau[1])
	),
	"rho[2]" = rnorm(
		n,
		hyperparams$log_rho_mu[2],
		pr2sd(hyperparams$log_rho_tau[2])
	),
	"rho[3]" = rnorm(
		n,
		hyperparams$log_rho_mu[3],
		pr2sd(hyperparams$log_rho_tau[3])
	),
	"rho[4]" = rnorm(
		n,
		hyperparams$log_rho_mu[4],
		pr2sd(hyperparams$log_rho_tau[4])
	),
	"rho[5]" = rnorm(
		n,
		hyperparams$log_rho_mu[5],
		pr2sd(hyperparams$log_rho_tau[5])
	),
	"gamma[1]" = rnorm(
		n,
		hyperparams$log_gamma_mu[1],
		pr2sd(hyperparams$log_gamma_tau[1])
	),
	"gamma[2]" = rnorm(
		n,
		hyperparams$log_gamma_mu[2],
		pr2sd(hyperparams$log_gamma_tau[2])
	),
	"p_mu[1]" = exp(rnorm(
		n,
		hyperparams$p_mu_mu[1],
		pr2sd(hyperparams$p_mu_tau[1])
	)),
	"p_mu[2]" = exp(rnorm(
		n,
		hyperparams$p_mu_mu[2],
		pr2sd(hyperparams$p_mu_tau[2])
	))
)

method_lookup_table <- tibble(
	method_idx = 1:5,
	method_names = c(
		"Ground-shooting",
		"Fixed Wing",
		"Helicopter",
		"Snare",
		"Trap"
	)
)

land_lookup <- tibble(
	position = 1:4,
	land_type = c("Intercept", "Road density", "Ruggedness", "Canopy cover")
)


priors_df <- priors |>
	pivot_longer(cols = everything(), names_to = "node") |>
	group_by(node) |>
	reframe(
		median = median(value),
		quantile_0.05 = quantile(value, 0.05),
		quantile_0.95 = quantile(value, 0.95)
	) |>
	mutate(
		st_name = "Prior",
		method_idx = case_when(
			grepl("\\[", node) ~ as.numeric(str_extract(node, method_pattern)),
			.default = NA
		),
		position = case_when(
			grepl("beta1", node) ~ 1,
			grepl("beta_p", node) ~ as.numeric(str_extract(node, position_pattern)) +
				1,
			.default = NA
		)
	) |>
	left_join(method_lookup_table, by = "method_idx") |>
	left_join(land_lookup, by = "position")

write_rds(priors_df, file.path("data", "priors_df.rds"))
