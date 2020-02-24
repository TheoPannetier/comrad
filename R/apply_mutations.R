#' Apply random mutations to a population
#'
#' The trait value of each individual in the input population is modified, with
#' probability `prob_mutation`, by a mutation sampled in a normal distribution
#' of mean `0` and variance `mutation_sd`.
#'
#' @inheritParams default_params_doc
#'
#' @author Theo Pannetier
#' @export

apply_mutations <- function(
  traits_pop,
  prob_mutation = default_prob_mutation(),
  mutation_sd = default_mutation_sd()
) {
  # Test arguments -------------------------------------------------------------
  comrad::testarg_num(prob_mutation)
  comrad::testarg_prop(prob_mutation)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)


  # Apply mutations ------------------------------------------------------------
  is_mutant <- stats::rbinom(length(traits_pop), 1, prob_mutation)
  mutations <- stats::rnorm(length(traits_pop), 0, mutation_sd)
  traits_pop <- ifelse(
    is_mutant,
    traits_pop + mutations,
    traits_pop
  )

  # Test output --------------------------------------------------------------
  comrad::testarg_num(traits_pop)

  return(traits_pop)
}
