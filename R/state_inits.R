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
		out$beta1 <- c(0, 2, -5.5, -3.25, NA)
		# fmt: skip
		out$beta_p <- c(
			0, 2, -1,
			0, 1.5, 0.5,
			0.25, -0.5, 1,
			0, 1, -0.2
		)
		out$p_mu <- c(-2, -3.5)
		out$log_gamma <- c(-1.5, -3.8)
		out$log_rho <- c(-1, 1, -2, 0.2)
		out$psi_phi <- c(0.7, 0.8)
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
	out
}

# Template if all methods used
# beta1 <- c(NA, NA, NA, NA, NA) means
# fmt: skip
# beta_p <- c( means by row
# 			NA, NA, NA,
# 			NA, NA, NA,
# 			NA, NA, NA,
# 			NA, NA, NA,
# 			NA, NA, NA
# 		)
# p_mu <- c(NA, NA) means
# log_gamma <- c(NA, NA) means
# log_rho <- c(NA, NA, NA, NA, NA) means
# psi_phi <-c(NA, NA) ranges
# phi_mu <- c(NA, NA) ranges
# log_nu <- c(NA, NA) ranges
