#' Draw a number of offspring given a fitness value
#'
#' The number of offspring is drawn in a Poisson distribution in
#' `rpois(lambda = fitness)`. Vectorized.
#'
#' @param fitness numeric `>= 0`, a vector of fitness values.
#'
#' @seealso get_fitness
#' @author Theo Pannetier
#' @export

draw_nb_offspring <- function(fitness) {
  comrad::testarg_num(fitness)
  comrad::testarg_pos(fitness)

  # Vectorize
  nb_offspring <- sapply(fitness, function(g) {
    stats::rpois(1, g)
  })

  nb_offspring
}
