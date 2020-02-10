#' Compute the effective population size
#'
#' Computes \eqn{n_eff}, the effective population size experienced by an
#' individual.
#'
#' @inheritParams default_params_doc
#' @export
#' @author Theo Pannetier

get_n_eff <- function(traits_pop, comp_width = default_comp_width()) {
  # Test arguments -------------------------------------------------------------
  testarg_num(traits_pop)
  testarg_num(comp_width)
  testarg_pos(comp_width) # is a variance

  # Sum competition coefficients for each individual ---------------------------
  n_eff <- sapply(
    X = traits_pop,
    FUN = function(trait_ind) {
      get_comp_coeff_pop(
        trait_ind = trait_ind,
        traits_pop = traits_pop, # ind competes against whole pop, incl. itself
        comp_width = comp_width
      ) %>% sum()
    }
  )

  # Test output ----------------------------------------------------------------
  testarg_num(n_eff)
  testarg_pos(n_eff)
  testarg_length(n_eff, length(traits_pop))
  if (any(n_eff > length(traits_pop))) { # N is the upper bound
    stop("'n_eff' became greater than population size.")
  }
  if (any(n_eff < 1)) { # 1 is the lower bound
    stop("'n_eff' became lower than 1.")
  }

  n_eff
}
