#' Computes the effective population size, but faster
#'
#' This function does the same job as [get_n_eff()], but faster, at the cost
#' of less straightforward code.
#'
#' @inheritParams default_params_doc
#'
#' @details For each individual, [get_n_eff()] computes the sum of its
#' interactions ([get_comp_coeffs()]) with every other individual in the
#' population. Because these interactions are symmetric
#' (i.e. `get_comp_coeff(x, y) == get_comp_coeff(y, x)`), [get_n_eff()] does
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
  testarg_num(comp_width)
  testarg_pos(comp_width) # is a variance

  # Create a table of all unique matches between individuals
  match_tbl <- utils::combn(traits_pop, 2) %>% t()
  colnames(match_tbl) <- c("ind_one", "ind_two")
  match_tbl <- match_tbl %>% dplyr::as_tibble()

  # Compute the competition coefficient for each possible match
  comp_coeffs <- purrr::map2(
    match_tbl$ind_one,
    match_tbl$ind_two,
    function(x, y) {
      get_comp_coeffs(
        trait_ind = x,
        traits_pop = y,
        comp_width = comp_width
      )
    }
  ) %>% unlist()

  n_eff <- purrr::map(
    traits_pop,
    function(ind) {
      # Find all matches in which the individual competed
      ind_matches <- c(
        which(match_tbl$ind_one == ind),
        which(match_tbl$ind_two == ind)
      )
      # Sum their outcome to get effective population size for that individual
      comp_coeffs[ind_matches] %>%
        sum() + 1 # ind competes against itself!
    }
  ) %>% unlist()

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

