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
  # TODO: include description with references
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
#' npv <- calc_npv(cf, 0.0064, 1000000)
calc_npv <- function(cf, r, c0 = 0) {
  # TODO: include description with references
  npv <- 0
  for (n in seq_along(cf)) {
    npv <- npv + calc_pv(cf[n], r, n) 
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
#' fv <- calc_fv(1000, 0.1, 5)
calc_fv <- function(pv, r, n) {
  # TODO: include description with references
  pv * (1 + r)^n
}
