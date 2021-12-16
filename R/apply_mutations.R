#' Apply random mutations to a community
#'
#' The trait value of each individual in the input community is modified by a mutation sampled in a normal distribution
#' of mean `0` and standard deviation `mutation_sd`.
#'
#' @inheritParams default_params_doc
#'
#' @author Theo Pannetier
#' @export

apply_mutations <- function(
  traits_comm,
  mutation_sd = default_mutation_sd()
) {
  # Test arguments -------------------------------------------------------------
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)

  # Apply mutations ------------------------------------------------------------
  traits_comm <- traits_comm + stats::rnorm(length(traits_comm), 0, mutation_sd)

  # Test output --------------------------------------------------------------
  comrad::testarg_num(traits_comm)

  return(traits_comm)
}
