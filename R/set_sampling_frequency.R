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
#'   \item if \code{nb_generations >= 1000}, sampling frequency is set to the
#'   order of magnitude of \code{nb_generations} - 2, e.g. 2 for
#'   \code{nb_generations >= 1000}, 3 for \code{nb_generations >= 10000}, etc.
#' }
#'
#' @author Theo Pannetier
#' @export

set_sampling_frequency <- function(nb_generations) {
  testarg_num(nb_generations)
  testarg_pos(nb_generations)

  max(1, 10 ^ floor(log10(nb_generations) - 2))
}
