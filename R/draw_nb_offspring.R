#' Draw a number of offsprings given a fitness value
#'
#' The number of offspring is drawn in a Poisson distribution with mean
#' \code{fitness}.
#'
#' @inheritParams default_params_doc
#'
#' @author Theo Pannetier
#' @export

draw_nb_offspring <- function(fitness) {
  comrad::testarg_num(fitness)
  comrad::testarg_pos(fitness)

  # Vectorize
  nb_offspring <- sapply(fitness, function(g) {
    stats::rpois(1, g)
  })

  comrad::testarg_num(nb_offspring)
  comrad::testarg_pos(nb_offspring)
  comrad::testarg_length(nb_offspring, length(fitness))

  nb_offspring
}
