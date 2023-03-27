#' Check that the requested DD model is available in `DDD`
#'
#' Calls `dd_sim()` from `DDD` to run a quick simulation and check
#' that the specified DD model is available on the version of DDD installed
#' on this computer.
#'
#' @param dd_model a `dd_model` list, the output of e.g. [dd_model_lc()],
#' [dd_model_xc()], etc.
#'
#' @author Theo Pannetier
#' @export
#'
check_dd_model_is_avail <- function(dd_model) {

  dd_model_DDD <- dd_model$DDD_name

  # Arbitrary parameter vals with low chance of branching
  pars <- c(0.001, 0.00001, 5)
  age <- 1

  both_rates_vary <- !stringr::str_detect(dd_model$name, "c")
  if (both_rates_vary) pars[4] <- 0

  # Run a quick sim to check if specified DD model causes error
  tryCatch({
    quick_sim <- DDD::dd_sim(
      pars = pars,
      age = age,
      ddmodel = dd_model_DDD
    )
  },
  error = function(e) {
    error_msg <- paste(
      "DD model", dd_model$name,
      "is not supported in the installed version of DDD.")
    stop(error_msg)
    }
  )
}
