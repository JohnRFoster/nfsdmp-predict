#' Function to get MCMC initial values depending on the state being modeled
#'
#' @title state_inits
#'
#' @param STATE_NAME The state being modeled, uppercase (i.e. "FLORIDA")
#' @details if no `STATE_NAME` is given, return NULL for each parameter, initial conditions based
#' on posterior distributions from the original fit in Foster et al. 2026
#'
#' @author John Foster

state_inits <- function(state_name) {
	out <- list()
	out$beta1 <- NULL
	out$beta_p <- NULL
	out$p_mu <- NULL
	out$log_gamma <- NULL
	out$log_rho <- NULL
	out$psi_phi <- NULL
	out$phi_mu <- NULL
	out$log_nu <- NULL

	# Florida ----
	if (state_name == "FLORIDA") {
		out$beta1 <- tribble(
			~min   , ~max   ,
			-2.36  ,  0.126 ,
			-0.182 ,  0.418 ,
			-7.58  , -5.1   ,
			-3.34  , -2.98
		)
		out$beta_p <- tribble(
			~min   , ~max   ,
			 0.381 ,  1.29  , # [1, 1]
			 0.586 ,  1.84  , # [1, 2]
			-1.15  ,  0.275 , # [1, 3]
			-0.05  ,  0.185 , # [2, 1]
			 0.192 ,  0.793 , # [2, 2]
			-0.725 , -0.21  , # [2, 3]
			-1.26  , -0.177 , # [3, 1]
			-3.03  , -0.821 , # [3, 2]
			-0.564 ,  0.785 , # [3, 3]
			 0.045 ,  0.167 , # [4, 1]
			 0.382 ,  0.55  , # [4, 2]
			-0.469 , -0.204 # [4, 3]
		)

		out$p_mu <- data.frame(
			min = c(-3.36, -0.3),
			max = c(0.721, 1.93)
		)
		out$log_gamma <- data.frame(
			min = c(-1.93, -0.376),
			max = c(-3.26, -2.45)
		)
		out$log_rho <- tribble(
			~min  , ~max   ,
			-1.08 ,  0.498 ,
			 1.28 ,  1.41  ,
			-2.52 , -1.26  ,
			 0.09 ,  0.341
		)
		out$psi_phi <- c(0.817, 0.976)
		out$phi_mu <- c(0.653, 0.69)
		out$log_nu <- c(2.31, 2.4)
	}

	# Georgia ----
	if (state_name == "GEORGIA") {
		out$beta1 <- tribble(
			~min , ~max ,
			-1   ,  0   ,
			 0   ,  0.5 ,
			-7   , -5   ,
			-3.5 , -3
		)
		out$beta_p <- tribble(
			~min  , ~max  ,
			 0.2  ,  1.5  , # [1, 1]
			 0.5  ,  1.5  , # [1, 2]
			-1    ,  0    , # [1, 3]
			 0    ,  0.2  , # [2, 1]
			 0.2  ,  0.8  , # [2, 2]
			-0.5  ,  0    , # [2, 3]
			-1.5  , -0.5  , # [3, 1]
			-3    , -1    , # [3, 2]
			-0.25 ,  0.25 , # [3, 3]
			 0    ,  0.2  , # [4, 1]
			 0.4  ,  0.6  , # [4, 2]
			-0.5  ,  0 # [4, 3]
		)

		out$p_mu <- data.frame(
			min = c(-3, 0.2),
			max = c(-1, 2)
		)
		out$log_gamma <- data.frame(
			min = c(-2, -3.5),
			max = c(-1, -2.5)
		)
		out$log_rho <- tribble(
			~min , ~max ,
			-1   , -0.2 ,
			 1.2 ,  1.4 ,
			-2.5 , -1.5 ,
			 0.1 ,  0.4
		)
		out$psi_phi <- c(0.9, 1.0)
		out$phi_mu <- c(0.65, 0.7)
		out$log_nu <- c(2.35, 2.4)
	}

	# Mississippi ----
	if (state_name == "MISSISSIPPI") {
		out$beta1 <- tribble(
			~min , ~max ,
			-3.5 , -2.5 ,
			 1.2 ,  2   ,
			-7   , -5   ,
			-3.2 , -3
		)
		out$beta_p <- tribble(
			~min , ~max ,
			-1   , -0.5 , # [1, 1]
			 0.5 ,  1.5 , # [1, 2]
			 0   ,  0.5 , # [1, 3]
			 0.1 ,  0.3 , # [2, 1]
			 2   ,  3   , # [2, 2]
			-0.5 , -0.1 , # [2, 3]
			-2   , -1   , # [3, 1]
			-3   , -1   , # [3, 2]
			 0   ,  1   , # [3, 3]
			 0.1 ,  0.3 , # [4, 1]
			 1.5 ,  2   , # [4, 2]
			 0   ,  0.2 # [4, 3]
		)

		out$p_mu <- data.frame(
			min = c(-1.5, -2.75),
			max = c(0.5, -2.25)
		)
		out$log_gamma <- data.frame(
			min = c(-2, -2.5),
			max = c(-1, -2)
		)
		out$log_rho <- tribble(
			~min , ~max ,
			 0.5 ,  0.8 ,
			 0.7 ,  0.9 ,
			-2.5 , -1.5 ,
			 0.1 ,  0.3
		)
		out$psi_phi <- c(0.8, 0.85)
		out$phi_mu <- c(0.625, 0.65)
		out$log_nu <- c(2.37, 2.43)
	}

	# Oklahoma ----
	if (state_name == "OKLAHOMA") {
		out$beta1 <- tribble(
			~min  , ~max  ,
			-0.28 ,  0.35 ,
			-3.92 , -3.7  ,
			-0.80 , -0.76 ,
			-3.93 , -3.34 ,
			-2.90 , -2.87
		)
		out$beta_p <- tribble(
			~min    , ~max    ,
			-0.240  , -0.138  ,
			 0.0209 ,  0.689  ,
			 0.667  ,  0.958  ,
			 0.390  ,  0.478  ,
			-0.333  , -0.109  ,
			-1.08   , -0.805  ,
			 0.0879 ,  0.121  ,
			 0.0130 ,  0.0856 ,
			-0.388  , -0.328  ,
			-0.152  ,  0.0261 ,
			-1.79   , -1.23   ,
			-0.0220 ,  0.276  ,
			 0.111  ,  0.129  ,
			 0.292  ,  0.327  ,
			-0.424  , -0.386
		)
		out$p_mu <- data.frame(
			min = c(-0.57, -0.11),
			max = c(0.26, 0.17)
		)
		out$log_gamma <- data.frame(
			min = c(-1.83, -3.55),
			max = c(-1.4, -3.28)
		)
		out$log_rho <- tribble(
			~min   , ~max   ,
			-1.31  , -1.01  ,
			 0.375 ,  0.385 ,
			 1.91  ,  1.93  ,
			-2.10  , -1.68  ,
			 0.173 ,  0.262
		)

		out$psi_phi <- c(0.673, 0.693)
		out$phi_mu <- c(0.583, 0.591)
		out$log_nu <- c(2.58, 2.60)
	}

	# Texas ----
	if (state_name == "TEXAS") {
		out$beta1 <- tribble(
			~min  , ~max  ,
			-0.5  ,  0.5  ,
			-4.12 , -3.50 ,
			 1.75 ,  2.25 ,
			-6    , -5    ,
			-3    , -2
		)
		out$beta_p <- tribble(
			~min   , ~max   ,
			-0.3   ,  0.3   , # [1, 1]
			 2.4   ,  3     , # [1, 2]
			-1.5   , -0.5   , # [1, 3]
			 0.31  ,  0.555 , # [2, 1]
			-0.547 ,  0.095 , # [2, 2]
			-1.304 , -0.559 , # [2, 3]
			-0.3   ,  0.3   , # [3, 1]
			 1     ,  2     , # [3, 2]
			 0     ,  0.5   , # [3, 3]
			 0     ,  0.5   , # [4, 1]
			-1     ,  0     , # [4, 2]
			 0.5   ,  1.5   , # [4, 3]
			 0.1   ,  0.2   , # [5, 1]
			 0.7   ,  0.9   , # [5, 2]
			-0.4   , -0.3 # [5, 3]
		)
		out$p_mu <- data.frame(
			min = c(-2, -4),
			max = c(-1, -3.5)
		)
		out$log_gamma <- data.frame(
			min = c(-2, -4.25),
			max = c(-1, -3.25)
		)
		out$log_rho <- tribble(
			~min   , ~max   ,
			-1.61  , -0.684 ,
			 0.367 ,  0.405 ,
			 0.9   ,  1     ,
			-2.49  , -1.26  ,
			 0.1   ,  0.3
		)

		out$psi_phi <- c(0.65, 0.7)
		out$phi_mu <- c(0.61, 0.625)
		out$log_nu <- c(2.49, 2.54)
	}

	# Georgia ----
	if (state_name == "GEORGIA") {
		out$beta1 <- tribble(
			~min , ~max ,
			-1   ,  0   ,
			 0   ,  0.5 ,
			-7   , -5   ,
			-3.5 , -3
		)
		out$beta_p <- tribble(
			~min  , ~max  ,
			 0.2  ,  1.5  , # [1, 1]
			 0.5  ,  1.5  , # [1, 2]
			-1    ,  0    , # [1, 3]
			 0    ,  0.2  , # [2, 1]
			 0.2  ,  0.8  , # [2, 2]
			-0.5  ,  0    , # [2, 3]
			-1.5  , -0.5  , # [3, 1]
			-3    , -1    , # [3, 2]
			-0.25 ,  0.25 , # [3, 3]
			 0    ,  0.2  , # [4, 1]
			 0.4  ,  0.6  , # [4, 2]
			-0.5  ,  0 # [4, 3]
		)

		out$p_mu <- data.frame(
			min = c(-3, 0.2),
			max = c(-1, 2)
		)
		out$log_gamma <- data.frame(
			min = c(-2, -3.5),
			max = c(-1, -2.5)
		)
		out$log_rho <- tribble(
			~min , ~max ,
			-1   , -0.2 ,
			 1.2 ,  1.4 ,
			-2.5 , -1.5 ,
			 0.1 ,  0.4
		)
		out$psi_phi <- c(0.9, 1.0)
		out$phi_mu <- c(0.65, 0.7)
		out$log_nu <- c(2.35, 2.4)
	}

	# Ohio ----
	if (state_name == "OHIO") {
		out$beta1 <- tribble(
			~min , ~max ,
			-1   ,  1   ,
			 0.5 ,  1.5 ,
			-5   , -3   ,
			-3   , -2
		)
		out$beta_p <- tribble(
			~min  , ~max  ,
			-1    , -1    , # [1, 1]
			 0    ,  2    , # [1, 2]
			 0    ,  2    , # [1, 3]
			-0.75 , -0.25 , # [2, 1]
			 1    ,  2    , # [2, 2]
			-0.4  ,  0.4  , # [2, 3]
			-2    , -1    , # [3, 1]
			-3    , -1    , # [3, 2]
			 0.25 ,  2    , # [3, 3]
			-0.5  ,  0    , # [4, 1]
			 0    ,  0.5  , # [4, 2]
			-1    , -0.5 # [4, 3]
		)
		out$p_mu <- data.frame(
			min = c(-1, 1),
			max = c(1, 3)
		)
		out$log_gamma <- data.frame(
			min = c(-2, -3),
			max = c(-1, -2)
		)
		out$log_rho <- tribble(
			~min , ~max ,
			-1   ,  1   ,
			 1.6 ,  2   ,
			-2.5 , -1   ,
			 0.1 ,  0.5
		)

		out$psi_phi <- c(0.8, 1)
		out$phi_mu <- c(0.66, 0.72)
		out$log_nu <- c(1.85, 2)
	}

	# Louisiana ----
	if (state_name == "LOUISIANA") {
		out$beta1 <- tribble(
			~min , ~max ,
			-1   ,  1   ,
			 1   ,  1.5 ,
			-6   , -4   ,
			-2.2 , -1.8
		)
		out$beta_p <- tribble(
			~min  , ~max ,
			 0.25 ,  1   , # [1, 1]
			 0    ,  2   , # [1, 2]
			-1    ,  0   , # [1, 3]
			-0.4  ,  0   , # [2, 1]
			 1    ,  2   , # [2, 2]
			-0.4  ,  0.2 , # [2, 3]
			-0.5  ,  0.5 , # [3, 1]
			-2    ,  0   , # [3, 2]
			 1    ,  2   , # [3, 3]
			 0.05 ,  0.2 , # [4, 1]
			 1.4  ,  1.6 , # [4, 2]
			-0.3  , -0.1 # [4, 3]
		)
		out$p_mu <- data.frame(
			min = c(-1, -2.25),
			max = c(0.5, -1.75)
		)
		out$log_gamma <- data.frame(
			min = c(-2, -2.4),
			max = c(-1, -2)
		)
		out$log_rho <- tribble(
			~min  , ~max  ,
			-0.5  ,  0    ,
			 0.68 ,  0.72 ,
			-2.5  , -1    ,
			 0.1  ,  0.4
		)

		out$psi_phi <- c(0.8, 1)
		out$phi_mu <- c(0.65, 0.675)
		out$log_nu <- c(2.35, 2.4)
	}
	out
}

# Template if all methods used
# out$beta1 <- tribble(
# 	~min , ~max ,
# 	  NA ,   NA ,
# 	  NA ,   NA ,
# 	  NA ,   NA ,
# 	  NA ,   NA
# )
# out$beta_p <- tribble(
# 	~min , ~max ,
# 	NA   , NA   , # [1, 1]
# 	NA   , NA   , # [1, 2]
# 	NA   , NA   , # [1, 3]
# 	NA   , NA   , # [2, 1]
# 	NA   , NA   , # [2, 2]
# 	NA   , NA   , # [2, 3]
# 	NA   , NA   , # [3, 1]
# 	NA   , NA   , # [3, 2]
# 	NA   , NA   , # [3, 3]
# 	NA   , NA   , # [4, 1]
# 	NA   , NA   , # [4, 2]
# 	NA   , NA   , # [4, 3]
# 	NA   , NA   , # [5, 1]
# 	NA   , NA   , # [5, 2]
# 	NA   , NA     # [5, 3]
# )

# out$p_mu <- data.frame(
# 	min = c(NA, NA),
# 	max = c(NA, NA)
# )
# out$log_gamma <- data.frame(
# 	min = c(NA, NA),
# 	max = c(NA, NA)
# )
# out$log_rho <- tribble(
# 	~min , ~max ,
# 	NA   ,    NA ,
# 	NA   ,    NA ,
# 	NA   ,    NA ,
# 	NA   ,    NA
# )
# out$psi_phi <- c(NA, NA)
# out$phi_mu <- c(NA, NA)
# out$log_nu <- c(NA, NA)
