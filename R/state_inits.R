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

	if (state_name == "FLORIDA") {
		out$beta1 <- data.frame(
			min = c(-1, 1, -4, -2),
			max = c(1, 3, -8, -5)
		)
		out$beta_p <- tribble(
			~min , ~max ,
			-1   ,  1   , # [1, 1]
			 1   ,  4   , # [1, 2]
			-0.5 , -3   , # [1, 3]
			-1   ,  1   , # [2, 1]
			 0   ,  3   , # [2, 2]
			-1   ,  1   , # [2, 3]
			 0   ,  1   , # [3, 1]
			-0.5 , -2   , # [3, 2]
			 0.5 ,  1.5 , # [3, 3]
			-1   ,  1   , # [4, 1]
			 0.5 ,  2   , # [4, 2]
			 0   , -1 # [4, 3]
		)

		out$p_mu <- data.frame(
			min = c(-2, -4),
			max = c(0, -2)
		)
		out$log_gamma <- data.frame(
			min = c(-2, -4),
			max = c(0, -2)
		)
		out$log_rho <- data.frame(
			min = c(-2, 0, -3, 0),
			max = c(0, 2, -1, 0.5)
		)
		out$psi_phi <- c(0.58, 0.63)
		out$phi_mu <- c(0.58, 0.63)
		out$log_nu <- c(2.5, 2.6)
	}

	if (state_name == "GEORGIA") {
		out$beta1 <- c(-1, 0.2, -6, -3.1)
		# fmt: skip
		out$beta_p <- c(
			1, 1.5, -0.5,
			0.1, 0.5, -0.5,
			-0.75, -2, 0,
			0.1, 0.45, -0.3
		)
		out$p_mu <- c(-2, 1.5)
		out$log_gamma <- c(-1, -2.75)
		out$log_rho <- c(-0.75, 1.35, -2, 0.2)
		out$psi_phi <- c(0.9, 1.0)
		out$phi_mu <- c(0.63, 0.7)
		out$log_nu <- c(2.35, 2.4)
	}

	if (state_name == "OKLAHOMA") {
		out$beta1 <- c(0.5, -3.75, -0.8, -3.5, -2.9)
		# fmt: skip
		out$beta_p <- c(
			-0.2, 0.5, 1,
			0.4, -0.25, -1,
			0.1, 0.5, -0.35,
			0, -1.5, 0,
			0.125, 0.3, -0.4
		)
		out$p_mu <- c(0, 0)
		out$log_gamma <- c(-1.5, -3.5)
		out$log_rho <- c(-1.25, 0.37, -1.92, -2, 0.2)
		out$psi_phi <- c(0.6, 0.8)
		out$phi_mu <- c(0.55, 0.6)
		out$log_nu <- c(2.55, 2.65)
	}
	out
}

# Template if all methods used
# beta1 <- c(NA, NA, NA, NA, NA) means
# fmt: skip
# out$beta_p <- tribble(
# 			~min , ~max ,
# 			NA   ,  NA  , # [1, 1]
# 			NA   ,  NA  , # [1, 2]
# 			NA   ,  NA  , # [1, 3]
# 			NA   ,  NA  , # [2, 1]
# 			NA   ,  NA  , # [2, 2]
# 			NA   ,  NA  , # [2, 3]
# 			NA   ,  NA  , # [3, 1]
# 			NA   ,  NA  , # [3, 2]
# 			NA   ,  NA  , # [3, 3]
# 			NA   ,  NA  , # [4, 1]
# 			NA   ,  NA  , # [4, 2]
# 			NA   ,  NA  , # [4, 3]
# 			NA   ,  NA  , # [5, 1]
# 			NA   ,  NA  , # [5, 2]
# 			NA   ,  NA    # [5, 3]
#		)
# p_mu <- c(NA, NA) means
# log_gamma <- c(NA, NA) means
# log_rho <- c(NA, NA, NA, NA, NA) means
# psi_phi <-c(NA, NA) ranges
# phi_mu <- c(NA, NA) ranges
# log_nu <- c(NA, NA) ranges
