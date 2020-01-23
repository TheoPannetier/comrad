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
default_comp_width <- function() {
  0.2
}

#' @export
#' @rdname default_pars
default_trait_opt <- function() {
  0
}

#' @export
#' @rdname default_pars
default_carr_cap_opt <- function() {
  1000
}

#' @export
#' @rdname default_pars
default_carr_cap_width <- function() {
  0.5
}

#' @export
#' @rdname default_pars
default_growth_rate <- function() {
  1
}

#' @export
#' @rdname default_pars
default_mutation_sd <- function() {
  0.02
}

#' @export
#' @rdname default_pars
default_init_pop <- function() {
  rep(0, 10) # ten individuals with optimal trait value (0)
}

#' @export
#' @rdname default_pars
default_seed <- function() {
  180
}
