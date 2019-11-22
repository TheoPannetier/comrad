#' Create offspring trait values from parent values
#'
#' Each parent first sires a number of offspring equal to its
#' \code{nb_offspring_pop} value, then each offspring receives a mutation
#' sampled in a normal distribution of mean \code{0} and variance
#' \code{mutation_sd}.
#'
#' @inheritParams default_params_doc
#' @param nb_offspring_pop numeric vector \code{>= 0}, the nb of offspring that
#' should be created for each parent.
#'
#' @author Theo Pannetier
#' @export

create_next_gen_traits <- function(
  traits_pop,
  nb_offspring_pop,
  mutation_sd = default_mutation_sd()
) {
  # Test arguments -------------------------------------------------------------
  testarg_num(traits_pop)
  testarg_num(nb_offspring_pop)
  testarg_not_this(nb_offspring_pop, Inf)
  testarg_length(nb_offspring_pop, length(traits_pop))
  testarg_num(mutation_sd)
  testarg_pos(mutation_sd)

  # Create new individuals and apply mutations --------------------------------
  next_gen_traits <- rep(traits_pop, nb_offspring_pop) # inherit parent trait
  next_gen_traits <- next_gen_traits +
    stats::rnorm(n = length(next_gen_traits), sd = mutation_sd)

  # Catch extinction
  if (length(next_gen_traits) < 1) {
    stop("Population has gone extinct")
  }
  # Test output ----------------------------------------------------------------
  testarg_num(next_gen_traits)
  testarg_length(next_gen_traits, sum(nb_offspring_pop))

  next_gen_traits
}
