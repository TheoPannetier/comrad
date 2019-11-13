#' Get the carrying capacity for a given trait value
#'
#' Computes the carrying capacity experienced by an individual with trait value
#' \code{trait}.
#'
#' @inheritParams default_params_doc
#'
#' @author Theo Pannetier
#' @export

carr_cap <- function(
  trait_ind,
  trait_opt = default_carr_cap_pars()[1],
  carr_cap_opt = default_carr_cap_pars()[2],
  carr_cap_var = default_carr_cap_pars()[3]
  ) {
  testarg_num(trait_ind)
  testarg_num(trait_opt)
  if (trait_ind %in% c(Inf, -Inf) && trait_ind == trait_opt) {
    stop("'trait_ind' and 'trait_opt' must not both be set to Inf or -Inf")
  } # causes a NaN
  testarg_num(carr_cap_opt)
  testarg_pos(carr_cap_opt) # is a nb of ind
  testarg_num(carr_cap_var)
  testarg_pos(carr_cap_var) # is a variance
  testarg_not_this(carr_cap_var, c(0, Inf)) # is the denominator

  trait_dist <- (trait_opt - trait_ind) ^ 2

  k <- carr_cap_opt * exp(- (trait_dist / (2 * carr_cap_var)))
  k
}
