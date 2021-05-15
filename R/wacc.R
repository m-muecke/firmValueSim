#' Load cost of capital data
#'
#' `load_wacc_data` loads Aswath Damodaran's latest cost of capital by industry
#' data.
#'
#' @param region 
#'
#' @return
#' @export
#'
#' @examples
load_wacc_data <- function(region = c("US", "Europe", "Japan", "China", "India",
                                      "Global")) {
  region <- match.arg(region)
  url_list <- list(
    US = "http://www.stern.nyu.edu/~adamodar/pc/datasets/wacc.xls",
    Europe = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccEurope.xls",
    Japan = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccJapan.xls",
    China = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccChina.xls",
    India = "http://www.stern.nyu.edu/~adamodar/pc/datasets/waccIndia.xls",
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
#' `load_beta_data` loads Aswath Damodaran's latest total beta by industry
#' data.
#' 
#' @param region 
#'
#' @return
#' @export
#'
#' @examples
load_beta_data <- function(region = c("US", "Europe", "Japan", "China", "India",
                                      "Global")) {
  region <- match.arg(region)
  url_list <- list(
    US = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbeta.xls",
    Europe = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaEurope.xls",
    Japan = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaJapan.xls",
    China = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaChina.xls",
    India = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaIndia.xls",
    Global = "http://www.stern.nyu.edu/~adamodar/pc/datasets/totalbetaGlobal.xls"
  )
  temp <- tempfile()
  download.file(url_list[[region]], temp, mode = "wb", quiet = TRUE)
  df <- readxl::read_excel(
    temp, range = "A8:G104"
  )
  df
}