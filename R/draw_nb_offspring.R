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
  testarg_num(fitness)
  testarg_pos(fitness)

  # Vectorize
  nb_offspring <- sapply(fitness, function(g) {
    stats::rpois(1, g)
  })

  testarg_num(nb_offspring)
  testarg_pos(nb_offspring)
  testarg_length(nb_offspring, length(fitness))

  nb_offspring
}
