#' Default parameter values
#'
#' Return default values of the simulation parameters, i.e. the default settings
#' used in Pontarp et al. (2012).
#'
#' @author Theo Pannetier
#' @name default_pars
NULL

#' @export
#' @rdname default_pars
default_sigma_comp <- function() {
  0.2
}

#' @export
#' @rdname default_pars
default_carr_cap_pars <- function() {
  c(
    0,    # trait_opt
    1000, # carr_cap_opt
    0.5   # carr_cap_var
  )
}

#' @export
#' @rdname default_pars
default_growth_rate <- function() {
  1
}
