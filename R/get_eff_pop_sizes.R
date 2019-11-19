#' Compute effective population sizes for a population
#'
#' Computes \eqn{n_eff}, the effective population size experienced by an
#' individual based on its trait value and the rest of the population.
#' Vectorized, can take either individual or population trait values as input.
#'
#' @inheritParams default_params_doc
#' @export
#' @author Theo Pannetier

get_eff_pop_sizes <- function(traits_pop, sigma_comp = default_sigma_comp()) {
  # Test arguments -------------------------------------------------------------
  testarg_num(traits_pop)
  testarg_num(sigma_comp)
  testarg_pos(sigma_comp) # is a variance

  # Sum competition coefficients for each individual ---------------------------
  eff_pop_sizes <- sapply(
    X = traits_pop,
    FUN = function(trait_ind) {
      comp_coeff(
        trait_ind = trait_ind,
        trait_comp = traits_pop, # ind competes against whole pop, incl. itself
        sigma_comp = sigma_comp
      ) %>% sum()
    }
  )

  # Test output ----------------------------------------------------------------
  testarg_num(eff_pop_sizes)
  testarg_pos(eff_pop_sizes)
  testarg_length(eff_pop_sizes, length(traits_pop))
  if (any(eff_pop_sizes > length(traits_pop))) { # N is the upper bound
    stop("'eff_pop_size' became greater than population size.")
  }
  if (any(eff_pop_sizes < 1)) { # 1 is the lower bound
    stop("'eff_pop_size' became lower than 1.")
  }

  eff_pop_sizes
}
