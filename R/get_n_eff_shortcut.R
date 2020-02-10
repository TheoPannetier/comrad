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
  ind_one <- NULL
  ind_two <- NULL
  outcome <- NULL

  # Test arguments -------------------------------------------------------------
  testarg_num(traits_pop)
  testarg_num(comp_width)
  testarg_pos(comp_width) # is a variance

  if (length(traits_pop) == 1) {
    # can't compute the combination matrix from that
    return(1)
  }

  # Create a table of all unique matches between individuals
  match_tbl <- utils::combn(seq_along(traits_pop), 2) %>% t()

  colnames(match_tbl) <- c("ind_one", "ind_two")
  match_tbl <- match_tbl %>% dplyr::as_tibble()

  # Compute the competition coefficient for each possible match
  match_tbl <- match_tbl %>% dplyr::mutate(
    "outcome" = get_comp_coeff_pair(
      trait_ind_one = traits_pop[ind_one],
      trait_ind_two = traits_pop[ind_two],
      comp_width = comp_width
    )
  )

  n_eff <- sapply(
    seq_along(traits_pop),
    function (ind) {
      match_tbl %>%
        dplyr::filter(ind_one == ind | ind_two == ind) %>%
        dplyr::select(outcome) %>%
        sum() + 1 # ind competes with itself !
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
