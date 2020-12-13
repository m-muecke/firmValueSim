#' Monte Carlo simulation for unlevered free cash flow model with perpetuity growth method
#'
#' @param n_sim 
#' @param CF 
#' @param FV 
#' @param shares 
#' @param wacc_mu 
#' @param wacc_sigma 
#' @param g_mu 
#' @param g_sigma 
#' @param years 
#' @param seed 
#' @param net_debt 
#'
#' @return
#' @export
#'
#' @examples
dcf_sim_norm <- function(n_sim, CF, FV, net_debt, shares, wacc_mu, wacc_sigma, g_mu,
                         g_sigma, years, seed = NULL){
  if (!is.null(seed)) {
    set.seed(seed)
  }
  wacc <- rnorm(n_sim, mean = wacc_mu, sd = wacc_sigma)
  g <- rnorm(n_sim, mean = g_mu, sd = g_sigma)
  share_values <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    g_t <- sample(g, size = 1, replace = TRUE)
    wacc_t <- sample(wacc, size = 1, replace = TRUE)
    all_pv <- FinCal::npv(r = wacc_t, cf = CF)
    fcf <- -FinCal::pv(r = wacc_t, fv = FV, n = years)
    fcf_1 <- fcf * (1 + g_t)
    terminal_value <- fcf_1 / (wacc_t - g_t)
    pv_terminal_value <- -pv(r = wacc_t, fv = terminal_value, n = years)
    enterprise_value <- pv_terminal_value + all_pv
    equity_value <- enterprise_value - net_debt
    value_per_share <- equity_value / shares
    share_values[i] <- value_per_share
  }
  message(boxplot.stats(share_values)$out)
  share_values
}