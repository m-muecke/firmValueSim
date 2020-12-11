#' Monte Carlo Simulation for the Discount Dividend model
#'
#' @param price 
#' @param dividend 
#' @param g_mu 
#' @param g_sigma 
#' @param n_sim 
#' @param distribution 
#'
#' @return
#' @export
#'
#' @examples
ddm_sim <- function(price, dividend, g_mu = NULL, g_sigma = NULL, g_min = NULL,
                    g_max = NULL, n_sim = 1000,
                    dist = c("normal", "triangle", "uniform")) {
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
    if (!is.null(g_min) && !is.null(a)) {
      g <- runif(n_sim, min = g_min, max = g_max)
    } else {
      stop("Minimum and/or maximium is missing for uniform distribution.")
    }
  } else {
    stop("Selected a distribution that is not available.")
  }
  share_values <- numeric(length(n_sim))
  for (i in seq_len(n_sim)) {
    g_t <- sample(g, size = 1, replace = TRUE)
    value_per_share <- price / (dividend - g_t)
    share_values[i] <- value_per_share
  }
  share_values
}