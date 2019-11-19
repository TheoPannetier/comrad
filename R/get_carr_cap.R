#' Get the carrying capacity for a given trait value
#'
#' Computes the carrying capacity experienced by an individual with trait value
#' \code{trait}.
#'
#' @inheritParams default_params_doc
#'
#' @author Theo Pannetier
#' @export

get_carr_cap <- function(
  trait_ind,
  trait_opt = default_carr_cap_pars()[1],
  carr_cap_opt = default_carr_cap_pars()[2],
  carr_cap_width = default_carr_cap_pars()[3]
  ) {
  testarg_num(trait_ind)
  testarg_num(trait_opt)
  testarg_num(carr_cap_opt)
  testarg_pos(carr_cap_opt) # is a nb of ind
  testarg_num(carr_cap_width)
  testarg_pos(carr_cap_width) # is a variance

  trait_dist <- (trait_opt - trait_ind) ^ 2
  if (trait_opt == Inf) {
    trait_dist[which(trait_ind == Inf)] <- 0 # replace NaNs with 0
  }

  k <- carr_cap_opt * exp(- (trait_dist / (2 * carr_cap_width)))

  # Solve possible NaN issues --------------------------------------------------
  # NaNs can arise if both terms in the division are equal to 0 or Inf
  if (carr_cap_width == 0) { # I rule that carr_cap_width has precedence
    nans <- which(trait_dist == 0)
    k[nans] <- carr_cap_opt # as if trait_dist / carr_cap_width = 0
  } else if (carr_cap_width == Inf) {# I rule that carr_cap_width has precedence
    nans <- which(trait_dist == Inf)
    k[nans] <- carr_cap_opt # as if trait_dist / carr_cap_width = 0
  }
  # NaNs can also arise if carr_cap_opt is set to Inf and the exp term is 0
  if (carr_cap_opt == Inf) { # I rule that carr_cap_opt has precedence
    nans <- which(exp(- (trait_dist / (2 * carr_cap_width))) == 0)
    k[nans] <- carr_cap_opt
  }

  testarg_num(k) # catch NAs, NaNs and NULL
  testarg_pos(k)
  testarg_length(k, length(trait_ind))

  k
}
