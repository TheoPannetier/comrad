#' Compute the effective population size
#'
#' Computes `n_eff`, the effective population size experienced by an
#' individual.
#'
#' @param z numeric, the trait value(s) to get the effective size for. NOte that
#' this does not need to be one of the values in `traits_comm`.
#' @param traits_comm numeric vector, the trait values of all individuals in the
#' community?
#' @inheritParams default_params_doc
#' @details `n_eff` sums the competitive effects an individual receives from
#' every individual in the community, including the individual itself. It is
#' called effective population size because it is the size of the population
#' that is relevant for competition.
#'
#' @export
#' @author Theo Pannetier

get_n_eff <- function(z,
                      traits_comm,
                      comp_width = default_comp_width()) {
  # Test arguments -------------------------------------------------------------
  comrad::testarg_num(z)
  comrad::testarg_not_this(z, c(Inf, -Inf))
  comrad::testarg_num(traits_comm)
  comrad::testarg_not_this(traits_comm, c(Inf, -Inf))
  comrad::testarg_num(comp_width)
  comrad::testarg_pos(comp_width) # is a variance

  # Sum competition coefficients for each individual ---------------------------
  n_eff <- sapply(
    X = z,
    FUN = function(trait_ind) {
      comp_coeff_comm <- comrad::get_comp_coeff_comm(
        trait_ind = trait_ind,
        traits_comm = traits_comm, #ind competes against whole pop, incl. itself
        comp_width = comp_width
      ) # includes competition of the individual against itself
      sum(comp_coeff_comm)
    }
  )

  # Test output ----------------------------------------------------------------
  comrad::testarg_num(n_eff)
  comrad::testarg_pos(n_eff)

  n_eff
}
