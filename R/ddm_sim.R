ddm_sim_norm <- function(price, dividend, g_mu, g_sigma, n_sim = 1000) {
  # monte carlo sim for ddm model with normal distribution of growth rate
  g <- rnorm(1:n_sim, mean = g_mu, sd = g_sigma)
  share_values <- numeric(length(n_sim))
  for (i in 1:n_sim) {
    g_t <- sample(g, size = 1, replace = TRUE)
    value_per_share <- price / (dividend - g_t)
    share_values[i] <- values_per_share
  }
  return(share_values)
}

ddm_sim_tri <- function(price, dividend, g_min, g_max, n_sim = 1000) {
  # monte carlo sim for ddm model with triangular distribution of growth rate
  library("triangle")
  g <- rtriangle(1:n_sim, a = g_min, b = g_max)
  share_values <- numeric(length(n_sim))
  for (i in 1:n_sim) {
    g_t <- sample(g, size = 1, replace = TRUE)
    value_per_share <- price / (dividend - g_t)
    share_values[i] <- values_per_share
  }
  return(share_values)
}

ddm_sim_unif <- function(price, dividend, g_min, g_max, n_sim = 1000) {
  # monte carlo sim for ddm model with uniform distribution of growth rate
  g <- runif(1:n_sim, min = g_min max = g_max)
  share_values <- numeric(length(n_sim))
  for (i in 1:n_sim) {
    g_t <- sample(g, size = 1, replace = TRUE)
    value_per_share <- price / (dividend - g_t)
    share_values[i] <- values_per_share
  }
  return(share_values)
}