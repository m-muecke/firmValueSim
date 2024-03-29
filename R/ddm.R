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
  }
  share_values <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    g_t <- sample(g, size = 1, replace = TRUE)
    share_values[i] <- ddm(d, r, g_t)
  }
  share_values
}

#' Dividend Discount Model (Gordon Growth Model)
#'
#' `ddm()` calculates the company stock price based on the dividend discount
#'  model. The dividend discount model is a method of valuing the stock price of
#'  a company based on the future dividend payments it will make, discounted
#'  back to their present value.
#'
#' @param d Numeric. Estimated value of dividend in the next year.
#' @param r Numeric. Constant cost of equity.
#' @param g Numeric. Constant growth rate for dividends in perpetuity.
#'
#' @return The estimated price per share.
#' @export
#'
#' @references
#' Gordon, M. J. (1959). Dividends, earnings, and stock prices.
#' The review of economics and statistics, 99-105.ISO 690

#' @examples
#' stock_price <- ddm(1.5, 0.05, 0.03)
ddm <- function(d, r, g) {
  if (r == 0 && g == 0) {
    stop("You've set cost of equity and the growth rate to zero.")
  }
  d / (r - g)
}

#' Dividend growth rate
#'
#' Growth rate equals the return on equity times the plowback ratio, or growth
#' is determined by how much of earnings is put back into the firm, and how
#' profitable those earnings are.
#'
#' @param roe Numeric. Return on equity.
#' @param b Numeric. Plowback ratio.
#'
#' @return The company's growth rate.
#' @export
#'
#' @examples
#' g <- growth_rate(0.25, 0.4)
growth_rate <- function(roe, b) {
  roe * b
}

#' Dividend plowback ratio.
#'
#' @param d Numeric. Dividend per share.
#' @param eps Numeric. Earnings per share.
#'
#' @return
#' @export
#'
#' @examples
#' b <- plowback_ratio(0.84, 5.73)
plowback_ratio <- function(d, eps) {
  1 - d / eps
}
