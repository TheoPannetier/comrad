#' Sample a fraction of the output of a `comrad` simulation
#'
#' Subset `output`, returning the number of rows (individuals) specified through
#' `sampling_frac`
#'
#' @param output the output of a single generation of [run_simulation()]
#' @param sampling_frac numeric (between 0 and 1), the proportion of
#' individuals. The fraction of individuals returned is approximative
#' because of a truncation.
#'
#' @author Théo Pannetier
#' @export
#'
sample_output <- function(output, sampling_frac = default_sampling_frac()) {

  comrad::test_comrad_comm(output[, 2:4])
  comrad::testarg_num(sampling_frac)
  comrad::testarg_prop(sampling_frac)

  nb_ind <- length(output$z)
  nb_sampled_ind <- trunc(nb_ind * sampling_frac)
  sampled_ind <- sample(x = 1:nb_ind, size = nb_sampled_ind)

  return(output[sampled_ind, ])
}
