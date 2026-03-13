#' Internal LBDiscover compatibility helpers
#'
#' These helpers call functions from LBDiscover without requiring
#' them to be exported in that package.
#'
#' @name lbdiscover_compat
#' @keywords internal
NULL

.lbdiscover_call <- function(fun_name, ...) {
  if (!requireNamespace("LBDiscover", quietly = TRUE)) {
    stop("Package 'LBDiscover' is required but not installed.")
  }

  ns <- asNamespace("LBDiscover")
  if (!exists(fun_name, envir = ns, inherits = FALSE)) {
    stop("Function '", fun_name, "' not found in LBDiscover namespace.")
  }

  fun <- get(fun_name, envir = ns, inherits = FALSE)
  fun(...)
}
