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

	# Oklahoma ----
	if (state_name == "OKLAHOMA") {
		out$beta1 <- tribble(
			~min , ~max ,
			-0.5 ,  0.5 ,
			-4   , -3.5 ,
			-1.5 , -1   ,
			-4   , -3   ,
			-3   , -2
		)
		out$beta_p <- tribble(
			~min  , ~max  ,
			-0.25 ,  0    , # [1, 1]
			 0    ,  0.5  , # [1, 2]
			 0.2  ,  1    , # [1, 3]
			 0.4  ,  0.6  , # [2, 1]
			-0.5  ,  0    , # [2, 2]
			-2    , -1    , # [2, 3]
			 0    ,  0.2  , # [3, 1]
			-0.1  ,  0.1  , # [3, 2]
			-1    ,  0    , # [3, 3]
			-0.2  ,  0.2  , # [4, 1]
			-2    , -1    , # [4, 2]
			-0.2  ,  0.2  , # [4, 3]
			 0    ,  0.2  , # [5, 1]
			 0    ,  0.5  , # [5, 2]
			-0.5  , -0.25 # [5, 3]
		)
		out$p_mu <- data.frame(
			min = c(-0.25, -0.25),
			max = c(0.25, 0.25)
		)
		out$log_gamma <- data.frame(
			min = c(-2, -4),
			max = c(-1, -3)
		)
		out$log_rho <- tribble(
			~min  , ~max ,
			-1.5  , -1   ,
			 0.3  ,  0.4 ,
			 1.75 ,  1.8 ,
			-2.5  , -1.5 ,
			 0.1  ,  0.3
		)

		out$psi_phi <- c(0.75, 0.85)
		out$phi_mu <- c(0.55, 0.6)
		out$log_nu <- c(2.55, 2.65)
	}

	# Texas ----
	if (state_name == "TEXAS") {
		out$beta1 <- tribble(
			~min   , ~max   ,
			-0.787 ,  1.13  ,
			-4.12  , -3.50  ,
			-0.834 , -0.724 ,
			-4.11  , -2.68  ,
			-2.93  , -2.85
		)
		out$beta_p <- tribble(
			~min   , ~max   ,
			-0.348 , -0.033 ,
			-0.564 ,  1.68  ,
			 0.412 ,  1.341 ,
			 0.31  ,  0.555 ,
			-0.547 ,  0.095 ,
			-1.304 , -0.559 ,
			 0.059 ,  0.147 ,
			-0.051 ,  0.152 ,
			-0.444 , -0.276 ,
			-0.31  ,  0.186 ,
			-2.292 , -0.751 ,
			-0.334 ,  0.54  ,
			 0.093 ,  0.147 ,
			 0.26  ,  0.36  ,
			-0.46  , -0.352
		)
		out$p_mu <- data.frame(
			min = c(-1.21, -0.37),
			max = c(1.19, 0.506)
		)
		out$log_gamma <- data.frame(
			min = c(-2.24, -3.79),
			max = c(-0.978, -3.09)
		)
		out$log_rho <- tribble(
			~min   , ~max   ,
			-1.61  , -0.684 ,
			 0.367 ,  0.405 ,
			 1.88  ,  1.96  ,
			-2.49  , -1.26  ,
			 0.086 ,  0.338
		)

		out$psi_phi <- c(0.654, 0.714)
		out$phi_mu <- c(0.575, 0.599)
		out$log_nu <- c(2.57, 2.62)
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
