#' Compute the effective population size
#'
#' Computes \eqn{n_eff}, the effective population size experienced by an
#' individual.
#'
#' @inheritParams default_params_doc
#' @export
#' @author Theo Pannetier

get_n_eff <- function(traits_comm, comp_width = default_comp_width()) {
  # Test arguments -------------------------------------------------------------
  comrad::testarg_num(traits_comm)
  comrad::testarg_not_this(traits_comm, c(Inf, -Inf))
  comrad::testarg_num(comp_width)
  comrad::testarg_pos(comp_width) # is a variance

  # Sum competition coefficients for each individual ---------------------------
  n_eff <- sapply(
    X = traits_comm,
    FUN = function(trait_ind) {
      comp_coeff_comm <- comrad::get_comp_coeff_comm(
        trait_ind = trait_ind,
        traits_comm = traits_comm, # ind competes against whole pop, incl. itself
        comp_width = comp_width
      ) # includes competition of the individual against itself
      sum(comp_coeff_comm)
    }
  )

  # Test output ----------------------------------------------------------------
  comrad::testarg_num(n_eff)
  comrad::testarg_pos(n_eff)
  comrad::testarg_length(n_eff, length(traits_comm))
  if (any(n_eff > length(traits_comm))) { # N is the upper bound
    stop("'n_eff' became greater than community size.")
  }
  if (any(n_eff < 1)) { # 1 is the lower bound
    stop("'n_eff' became lower than 1.")
  }

  n_eff
}
