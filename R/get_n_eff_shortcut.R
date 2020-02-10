#' Computes the effective population size with less computations
#'
#' This function is intended to do the same job as [get_n_eff()], but with less
#' computations.
#'
#' @inheritParams default_params_doc
#'
#' @details For each individual, [get_n_eff()] computes the sum of its
#' interactions ([get_comp_coeff_pair()]) with every other individual in the
#' population. Because these interactions are symmetric
#' (i.e. `comp_coeff(x, y) == comp_coeff(y, x)`), [get_n_eff()] does
#' more calculations than is actually necessary. `get_n_eff_shortcut()` here is
#' its lazy twin, that computes the competition coefficient once for each unique
#' pair of individuals.
#'
#' @export
#' @author Theo Pannetier
#'
get_n_eff_shortcut <- function(traits_pop, comp_width = default_comp_width()) {
  # Test arguments -------------------------------------------------------------
  testarg_num(traits_pop)
  testarg_not_this(traits_pop, c(Inf, -Inf))
  testarg_num(comp_width)
  testarg_pos(comp_width)

  # stupid, but necessary to avoid NOTE
  ind_one <- NULL
  ind_two <- NULL
  outcome <- NULL

  if (length(traits_pop) == 1) {
    # can't compute the combination matrix from that
    return(1)
  }

  # Create a table of all unique matches between individuals
  match_tbl <- tibble::as_tibble(t(utils::combn(seq_along(traits_pop), 2)))
  colnames(match_tbl) <- c("ind_one", "ind_two")
  #match_tbl <- match_tbl %>% dplyr::as_tibble()

  # Compute the competition coefficient for each possible match
  match_tbl <- dplyr::mutate(
    match_tbl,
    "outcome" = get_comp_coeff_pair(
      trait_ind_one = traits_pop[ind_one],
      trait_ind_two = traits_pop[ind_two],
      comp_width = comp_width
    )
  )

  testarg_num(match_tbl$outcome)
  testarg_prop(match_tbl$outcome)

  n_effs <- sapply(
    seq_along(traits_pop),
    function (ind) {
      comp_coeffs_ind <- dplyr::select(
        dplyr::filter(
          match_tbl,
          ind_one == ind | ind_two == ind
        ),
        outcome
      )
      n_eff <- sum(comp_coeffs_ind) + 1 # ind competes with itself !
    }
  )

  # Test output ----------------------------------------------------------------
  testarg_num(n_effs)
  testarg_pos(n_effs)
  testarg_length(n_effs, length(traits_pop))
  if (any(n_effs > length(traits_pop))) { # N is the upper bound
    stop("'n_effs' became greater than population size.")
  }
  if (any(n_effs < 1)) { # 1 is the lower bound
    stop("'n_effs' became lower than 1.")
  }
  n_effs
}
