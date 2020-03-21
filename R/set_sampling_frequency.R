#' Set default sampling frequency
#'
#' Returns an appropriate sampling frequency based on the order of magnitude of
#' \code{nb_generations}.
#'
#' @inheritParams default_params_doc
#'
#' @details The sampling frequency is set as follows:
#' \itemize{
#'   \item if \code{nb_generations < 1000}, sampling frequency is set to 1
#'   (every generation).
#'   \item if \code{nb_generations >= 1000}, sampling frequency is set to twice
#'   the order of magnitude of \code{nb_generations} - 2. E.g. for
#'   \code{nb_generations >= 1000}, the community is sampled every 20
#'   generations, every 200 generations for \code{nb_generations >= 10000},
#'   etc.
#' }
#'
#' @author Theo Pannetier
#' @export

set_sampling_frequency <- function(nb_generations) {
  comrad::testarg_num(nb_generations)
  comrad::testarg_pos(nb_generations)

  max(1, 2 * 10 ^ floor(log10(nb_generations) - 2))
}
