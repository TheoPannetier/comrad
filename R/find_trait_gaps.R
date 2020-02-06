#' Find gaps in trait values
#'
#' Runs through an ordered vector of trait values, returns the positions of gaps
#' between consecutive values.
#'
#' @param traits a numeric vector, **ordered** by ascending values.
#'
#' @author Théo Pannetier
#' @export

find_trait_gaps <- function(traits) {
  testarg_num(traits)
  if (any(traits != sort(traits))) {
    stop("'traits' must be ordered by ascending order before checking for gaps.")
  }

  trait_dist <- lapply(
      X = 1:(length(traits) - 1),
      FUN = function(ind) {
        abs(traits[ind] - traits[ind + 1])
      }
    ) %>% unlist()

  gaps <- which(trait_dist >= 0.1)
  gaps
}
