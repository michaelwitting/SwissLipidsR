#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  base_url <- "https://www.swisslipids.org/api/"
  assign("BASE_URL", base_url, envir = asNamespace(pkgname))
}
