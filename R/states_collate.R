library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# have not converged
skip_states <- c("TEXAS")
write_dir <- "analysis/states/states-2026-03-25"
out_dir <- "out/states/states-2026-03-25"
states <- list.files(out_dir)
states <- setdiff(states, skip_states)

analysis_dir <- "analysis"
density_fname <- "densitySummaries.rds"
param_fname <- "posteriorSamples.rds"

method_pattern <- "(?<=\\[)\\d"
position_pattern <- "(?<=\\, )\\d"

land_lookup <- tibble(
	position = 1:4,
	land_type = c("Intercept", "Road density", "Ruggedness", "Canopy cover")
)

all_params <- tibble()
all_params_samples <- tibble()
all_density_samples <- tibble()
all_density <- tibble()

for (i in seq_along(states)) {
	state <- str_to_title(states[i])
	state_dir <- file.path(out_dir, states[i])
	method_lookup_table <- read_rds(file.path(
		state_dir,
		"method_lookup_table.rds"
	))

	analysis_files <- list.files(file.path(state_dir, analysis_dir))

	# parameters ----
	fname <- file.path(state_dir, analysis_dir, param_fname)
	df_params <- read_rds(fname) |>
		mutate(
			st_name = state,
			log_zeta = log(28) + log_nu - log(365),
			lambda = phi_mu + exp(log_zeta) / 2,
			lambda_annual = lambda^(365 / 28),
		)

	quants <- df_params |>
		pivot_longer(cols = -st_name, names_to = "node") |>
		mutate(
			value = case_when(
				grepl("beta1", node) ~ boot::inv.logit(value),
				grepl("p_mu", node) ~ boot::inv.logit(value),
				grepl("log", node) ~ exp(value),
				.default = value
			),
			node = case_when(
				grepl("log", node) ~ str_remove(node, "log_"),
				.default = node
			),
		) |>
		group_by(node, st_name) |>
		reframe(
			median = median(value),
			quantile_0.05 = quantile(value, 0.05),
			quantile_0.95 = quantile(value, 0.95)
		) |>
		mutate(
			method_idx = case_when(
				grepl("\\[", node) ~ as.numeric(str_extract(
					node,
					method_pattern
				)),
				.default = NA
			),
			position = case_when(
				grepl("beta1", node) ~ 1,
				grepl("beta_p", node) ~ as.numeric(str_extract(
					node,
					position_pattern
				)) +
					1,
				.default = NA
			)
		)

	ts_specific_params <- c("gamma[1]", "gamma[2]", "p_mu[1]", "p_mu[2]")

	quants_shooting <- quants |>
		filter(!node %in% ts_specific_params) |>
		left_join(method_lookup_table, by = "method_idx") |>
		left_join(land_lookup, by = "position") |>
		select(-m_vec, -ts_id, -method_idx)

	quants_ts <- quants |>
		filter(node %in% ts_specific_params) |>
		rename(ts_id = method_idx) |>
		left_join(method_lookup_table, by = "ts_id") |>
		left_join(land_lookup, by = "position") |>
		select(-m_vec, -method_idx, -ts_id)

	quant_names <- bind_rows(quants_shooting, quants_ts)

	all_params <- bind_rows(all_params, quant_names)

	# Density summaries ----
	fname <- file.path(state_dir, analysis_dir, density_fname)
	df_density <- read_rds(fname) |>
		select(-node) |>
		mutate(st_name = state)

	all_density <- bind_rows(all_density, df_density)

	draws <- floor(seq(1, nrow(df_params), length.out = 500))

	all_params_samples <- bind_rows(all_params_samples, df_params[draws, ])

	density_samples <- read_rds(file.path(
		state_dir,
		analysis_dir,
		"stateSamples.rds"
	)) |>
		mutate(st_name = state)

	all_density_samples <- bind_rows(
		all_density_samples,
		density_samples[draws, ]
	)
}

# fix method names
all_params <- all_params |>
	mutate(
		method_names = stringr::str_to_title(method_names),
		method_names = case_when(
			method_names == "Firearms" ~ "Ground-shooting",
			st_name == "Wisconsin" ~ "Traps", # WI only uses traps
			st_name == "New York" & node == "gamma" ~ "Traps", # NY does not use snares
			st_name == "New York" & node == "p_mu" ~ "Traps", # NY does not use snares
			st_name == "Pennsylvania" & node == "gamma" ~ "Traps", # PA does not use snares
			st_name == "Pennsylvania" & node == "p_mu" ~ "Traps", # PA does not use snares
			.default = method_names
		)
	)

message("write_dir: ", write_dir)
if (!dir.exists(write_dir)) {
	dir.create(write_dir, recursive = TRUE)
}

message("writing all_params.rds and all_density.rds to: ", write_dir)
write_rds(all_params, file.path(write_dir, "all_params.rds"))
write_rds(all_params_samples, file.path(write_dir, "all_params_samples.rds"))
write_rds(all_density, file.path(write_dir, "all_density.rds"))
write_rds(
	all_density_samples,
	file.path(write_dir, "all_abundance_samples.rds")
)

message("done!")
