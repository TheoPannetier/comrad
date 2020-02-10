#' Get the competition coefficient between two individuals
#'
#' Alternative to [get_comp_coeff_pop()] on a single pair of individuals,
#' intended to save time on the number of computations. Note that the method of
#' computation is different between the two functions, so [get_comp_coeff_pop()]
#' is *not* a vectorized version of `get_comp_coeff_pair()`.
#'
#' @inheritParams default_params_doc
#' @param trait_ind_one numeric, trait value of a focal individual.
#' @param trait_ind_two numeric, trait value of a competign individual.
#'
#' @export
#' @author Théo Pannetier

get_comp_coeff_pair <- function(
  trait_ind_one,
  trait_ind_two,
  comp_width = default_comp_width()) {

  testarg_num(trait_ind_one)
  testarg_num(trait_ind_two)
  testarg_num(comp_width)
  testarg_pos(comp_width) # is a variance

  if (trait_ind_one %in% c(Inf, -Inf) && trait_ind_one == trait_ind_two) {
    trait_dist <- 0
  } else {
    trait_dist <- (trait_ind_one - trait_ind_two) ^ 2
  }

  comp_coeff <- exp(- (trait_dist / (2 * comp_width ^ 2)))

  # Solve possible NaN issues --------------------------------------------------
  # NaNs can arise if both terms in the division are equal to 0 or Inf
  if (comp_width == 0 && trait_dist == 0) {
    comp_coeff <- 1 # I rule that comp_width has precedence
  } else if (comp_width == Inf && trait_dist == Inf) {
    comp_coeff <- 0 # I rule that comp_width has precedence
  }

  testarg_num(comp_coeff)
  testarg_prop(comp_coeff)

  comp_coeff
}
