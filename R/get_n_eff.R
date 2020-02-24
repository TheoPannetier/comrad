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
  comrad::testarg_num(traits_pop)
  comrad::testarg_not_this(traits_pop, c(Inf, -Inf))
  comrad::testarg_num(comp_width)
  comrad::testarg_pos(comp_width) # is a variance

  # Sum competition coefficients for each individual ---------------------------
  n_eff <- sapply(
    X = traits_pop,
    FUN = function(trait_ind) {
      comp_coeff_pop <- comrad::get_comp_coeff_pop(
        trait_ind = trait_ind,
        traits_pop = traits_pop, # ind competes against whole pop, incl. itself
        comp_width = comp_width
      ) # includes competition of the individual against itself
      sum(comp_coeff_pop)
    }
  )

  # Test output ----------------------------------------------------------------
  comrad::testarg_num(n_eff)
  comrad::testarg_pos(n_eff)
  comrad::testarg_length(n_eff, length(traits_pop))
  if (any(n_eff > length(traits_pop))) { # N is the upper bound
    stop("'n_eff' became greater than population size.")
  }
  if (any(n_eff < 1)) { # 1 is the lower bound
    stop("'n_eff' became lower than 1.")
  }

  n_eff
}
