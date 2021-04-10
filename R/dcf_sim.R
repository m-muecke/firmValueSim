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
#'
#' @examples
dcf_sim_norm <- function(n_sim, CF, future_value, net_debt, shares, wacc_mu, wacc_sigma, g_mu,
                         g_sigma, years, seed = NULL){
  # TODO: fix me and include test
  if (!is.null(seed)) {
    set.seed(seed)
  }
  wacc <- rnorm(n_sim, mean = wacc_mu, sd = wacc_sigma)
  g <- rnorm(n_sim, mean = g_mu, sd = g_sigma)
  share_values <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    g_t <- sample(g, size = 1, replace = TRUE)
    wacc_t <- sample(wacc, size = 1, replace = TRUE)
    all_pv <- calc_npv(CF, wacc_t)
    fcf <- calc_pv(future_value, wacc_t, years)
    fcf_1 <- fcf * (1 + g_t)
    terminal_value <- fcf_1 / (wacc_t - g_t)
    pv_terminal_value <- calc_pv(terminal_value, wacc_t, years)
    enterprise_value <- pv_terminal_value + all_pv
    equity_value <- enterprise_value - net_debt
    value_per_share <- equity_value / shares
    share_values[i] <- value_per_share
  }
  message(boxplot.stats(share_values)$out)
  share_values
}

#' Present Value
#'
#' @param fv Numeric. Future value.
#' @param r Numeric. Rate of return.
#' @param n Numeric. Number of periods.
#'
#' @return The calculated present value.
#' @export
#'
#' @examples
#' pv <- calc_pv(2000, 0.03, 3)
calc_pv <- function(fv, r, n) {
  # TODO: include tests
  fv / (1 + r)^n
}

#' Net Present Value
#'
#' @param cf Numeric. Cash flow for each period.
#' @param r Numeric. Cost of capital.
#' @param c0 Numeric. Initial investment. Default 0.
#'
#' @return Rate of return.
#' @export
#'
#' @examples
#' cf <- rep(25000, 60)
#' npv <- calc_npv(1000000, cf, 0.0064)
calc_npv <- function(cf, r, c0 = 0) {
  # TODO: include tests
  npv <- 0
  for (i in seq_along(cf)) {
    npv <- npv + calc_pv(cf[i], r, i) 
  }
  npv - c0
}

#' Future Value
#'
#' @param pv Numeric. Present value.
#' @param r Numeric. Rate of return.
#' @param n Numeric. Number of periods.
#'
#' @return Future value.
#' @export
#'
#' @examples
#' pv <- calc_fv(1000, 0.1, 5)
calc_fv <- function(pv, r, n) {
  # TODO: include tests
  pv * (1 + r)^n
}
