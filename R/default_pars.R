#' Default parameter values
#'
#' Return default values of the simulation parameters, set after the default
#' settings used in Pontarp et al. (2012).
#'
#' @author Theo Pannetier
#' @name default_pars
NULL

#' @export
#' @rdname default_pars
default_competition_sd <- function() {
  0.2
}

#' @export
#' @rdname default_pars
default_trait_opt <- function() {
  0
}

#' @export
#' @rdname default_pars
default_carrying_cap_opt <- function() {
  1000
}

#' @export
#' @rdname default_pars
default_carrying_cap_sd <- function() {
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
  0.001
}

#' @export
#' @rdname default_pars
default_init_comm <- function() {
  tibble::tibble(
    "t" = rep(0, 10),
    "z" = rep(0, 10), # ten individuals with optimal trait value (0)
    "species" = "#89ae8a",
    "ancestral_species" = as.character(NA)
    )
}

#' @export
#' @rdname default_pars
default_seed <- function() {
  sample(1:40000, 1)
}

#' @export
#' @rdname default_pars
default_sampling_frac <- function() {
  0.05
}

#' @export
#' @rdname default_pars
default_trait_dist_sp <- function() {
  0.1
}
