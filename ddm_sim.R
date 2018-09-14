monte_carlo <- function(price, dividend, g_mu, g_sigma, n_sim) {
  ## monte carlo sim for unlevered free cash flow model with perpetuity growth method
  g <- rnorm(1:n_sim, mean = g_mu, sd = g_sigma)
  share_values <- numeric(length(n_sim))
  for (i in 1:n_sim) {
    g_t <- sample(g, size = 1, replace = TRUE)
    value_per_share <- price / (dividend - g_t)
    share_values[i] <- values_per_share
  }
  return(share_values)
}