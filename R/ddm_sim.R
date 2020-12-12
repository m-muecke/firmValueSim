#' Monte Carlo Simulation for the dividend discount model (Gordon Growth model)
#'
#' @param price The stock price of the company.
#' @param dividend The distributed dividend for the stock.
#' @param g_mu The mean of the normal distribution.
#' @param g_sigma The standard deviation of the normal distribution.
#' @param n_sim The number of simulations.
#' @param distribution The type of distribution.
#' @param g_min Lower limit of the distribution.
#' @param g_max Upper limit of the distribution.
#'
#' @return A numeric vector with the current simulated stock prices.
#' @export
#'
#' @examples
#' dividends <- ddm_sim(25.0, 1.5, g_mu = 0.03, g_sigma = 0.01)
ddm_sim <- function(price, dividend, g_mu = NULL, g_sigma = NULL, g_min = NULL,
                    g_max = NULL, n_sim = 1000,
                    distribution = c("normal", "triangle", "uniform")) {
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
    value_per_share <- price / (dividend - g_t)
    share_values[i] <- value_per_share
  }
  share_values
}