#' Monte Carlo Simulation for the dividend discount model (Gordon Growth model)
#'
#' @param d Numeric. Estimated value of dividend in the next year.
#' @param r Numeric. Constant cost of equity.
#' @param g_mu Numeric. The mean of the normal distribution.
#' @param g_sigma Numeric. The standard deviation of the normal distribution.
#' @param n_sim Numeric. The number of simulations.
#' @param distribution Character. The type of distribution.
#' @param g_min Numeric. Lower limit of the distribution.
#' @param g_max Numeric. Upper limit of the distribution.
#' @param seed Numeric. State for random number generation.
#'
#' @return A numeric vector with the current simulated stock prices.
#' @export
#'
#' @examples
#' stock_prices <- ddm_sim(1.5, 0.05, g_mu = 0.03, g_sigma = 0.01)
ddm_sim <- function(d, r, g_mu = NULL, g_sigma = NULL, g_min = NULL,
                    g_max = NULL, n_sim = 1000, seed = NULL,
                    distribution = c("normal", "triangle", "uniform")) {
  # TODO: include description with references
  if (n_sim <= 0) {
    stop("Number of simulations must be greater than one.")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  distribution <- match.arg(distribution)
  if (distribution == "normal") {
    if (!is.null(g_mu) && !is.null(g_sigma)) {
      g <- rnorm(n_sim, mean = g_mu, sd = g_sigma)
    } else {
      stop("Mean and/or standard deviation is missing for normal distribution.")
    }
  } else if (distribution == "triangle") {
    if (!is.null(g_min) && !is.null(g_max)) {
      g <- triangle::rtriangle(n_sim, a = g_min, b = g_max)
    } else {
      stop("Minimum and/or maximium is missing for triangle distribution.")
    }
  } else if (distribution == "uniform") {
    if (!is.null(g_min) && !is.null(g_max)) {
      g <- runif(n_sim, min = g_min, max = g_max)
    } else {
      stop("Minimum and/or maximium is missing for uniform distribution.")
    }
  } else {
    stop("Selected a distribution that is not available.")
  }
  share_values <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    g_t <- sample(g, size = 1, replace = TRUE)
    share_values[i] <- ddm(d, r, g_t)
  }
  share_values
}

#' Gordon Growth Model
#'
#' @param d Numeric. Estimated value of dividend in the next year.
#' @param r Numeric. Constant cost of equity.
#' @param g Numeric. Constant growth rate for dividends in perpetuity.
#'
#' @return The estimated price per share.
#' @export
#'
#' @examples
#' stock_price <- ddm(1.5, 0.05, 0.03)
ddm <- function(d, r, g) {
  # TODO: include description with references
  if (r == 0 && g == 0) {
    stop("You've set cost of equity and the growth rate to zero.")
  }
  d / (r - g)
}
