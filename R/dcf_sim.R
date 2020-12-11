dcf_sim_norm <- function(n_sim, CF, FV, debt, shares, wacc_mu, wacc_sigma, g_mu,
                         g_sigma, years, seed = NULL){
  ## monte carlo sim for unlevered free cash flow model with perpetuity growth method
  if (!is.null(seed)) {
    set.seed(seed)
  }
  wacc <- rnorm(n_sim, mean = wacc_mu, sd = wacc_sigma)
  g <- rnorm(n_sim, mean = g_mu, sd = g_sigma)
  share_values <- numeric(length(n_sim))
  for (i in 1:n_sim) {
    g_t <- sample(g, size = 1, replace = TRUE)
    wacc_t <- sample(wacc, size = 1, replace = TRUE)
    all_pv <- FinCal::npv(r = wacc_t, cf = CF)
    fcf <- -FinCal::pv(r = wacc_t, fv = FV, n = years)
    fcf_1 <-  fcf * (1 + g_t)
    terminal_value <- fcf_1 / (wacc_t - g_t)
    pv_terminal_value <- -pv(r = wacc_t, fv = terminal_value, n = years)
    enterprise_value <- pv_terminal_value + all_pv
    net_debt <- debt
    equity_value <- enterprise_value - net_debt
    shares <- shares
    value_per_share <- equity_value / shares
    share_values[i] <- value_per_share
  }
  message(boxplot.stats(share_values)$out)
  share_values
}

distribution_plot <- function(){
  values <-  dcf_sim_norm()
  hist(values, probability = FALSE,
       main = "Distribution function of simulated stock prices",
       xlab = "", ylab = "",cex.lab = 2.5, cex.axis = 2.5, cex.main = 2,
       cex.sub = 1.5, breaks = 25)
}

density_plot <- function(){
  values <- dcf_sim_norm()
  hist(values, probability = TRUE,
       main = "Density function of simulated stock prices",
       xlab = "", ylab = "", cex.lab = 2.5, cex.axis = 2.5,
       cex.main = 2, cex.sub = 1.5, breaks = 25)
  density <- density(values)
  lines(density, col = "red", lwd = 2)
}