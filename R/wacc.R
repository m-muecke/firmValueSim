#' Load cost of capital data
#'
#' `load_wacc` loads Aswath Damodaran's latest cost of capital by industry
#' data.
#'
#' @param region 
#'
#' @return
#' @export
#'
#' @examples
load_wacc <- function(region = c("US", "Europe", "Japan", "China", "India",
                                 "Emerging Markets", "Global")) {
  region <- match.arg(region)
  url_list <- list(
    US = "http://www.stern.nyu.edu/~adamodar/pc/datasets/wacc.xls",
    Europe = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccEurope.xls",
    Japan = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccJapan.xls",
    China = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccChina.xls",
    India = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccIndia.xls",
    `Emerging Markets` = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccemerg.xls",
    Global = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccGlobal.xls"
  )
  temp <- tempfile()
  download.file(url_list[[region]], temp, mode = "wb", quiet = TRUE)
  df <- readxl::read_excel(
    temp, range = "A19:L115"
  )
  df
}

#' Load total beta data
#'
#' `load_beta` loads Aswath Damodaran's latest total beta by industry
#' data.
#' 
#' @param region 
#'
#' @return
#' @export
#'
#' @examples
load_beta <- function(region = c("US", "Europe", "Japan", "China", "India",
                                 "Emerging Markets", "Global")) {
  region <- match.arg(region)
  url_list <- list(
    US = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbeta.xls",
    Europe = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaEurope.xls",
    Japan = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaJapan.xls",
    China = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaChina.xls",
    India = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaIndia.xls",
    `Emerging Markets` = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaemerg.xls",
    Global = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaGlobal.xls"
  )
  temp <- tempfile()
  download.file(url_list[[region]], temp, mode = "wb", quiet = TRUE)
  df <- readxl::read_excel(
    temp, range = "A8:G104"
  )
  df
}

#' Load fundamental growth rate in EPS data
#' 
#' `load_eps_growth_rate` loads Aswath Damodaran's latest fundamental growth
#' rate in EPS by industry data.
#'
#' @param region 
#'
#' @return
#' @export
#'
#' @examples
load_eps_growth_rate <- function(region = c("US", "Europe", "Japan",
                                            "Emerging Markets", "Global")) {
  region <- match.arg(region)
  url_list <- list(
    US = "http://www.stern.nyu.edu/~adamodar/pc/datasets/fundgr.xls",
    Europe = "http://www.stern.nyu.edu/~adamodar/pc/datasets/fundgrEurope.xls",
    Japan = "http://www.stern.nyu.edu/~adamodar/pc/datasets/fundgrJapan.xls",
    `Emerging Markets` = "http://www.stern.nyu.edu/~adamodar/pc/datasets/fundgremerg.xls",
    Global = "http://www.stern.nyu.edu/~adamodar/pc/datasets/fundgrGlobal.xls"
  )
  temp <- tempfile()
  download.file(url_list[[region]], temp, mode = "wb", quiet = TRUE)
  df <- readxl::read_excel(
    temp, range = "A8:E104"
  )
  df
}