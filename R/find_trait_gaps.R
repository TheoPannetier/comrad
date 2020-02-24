#' Find gaps in trait values
#'
#' Runs through an ordered vector of trait values, returns the positions of gaps
#' `>= 0.1` between consecutive values.
#'
#' @param traits a numeric vector, trait values **in ascending order**.
#'
#' @author Théo Pannetier
#' @export

find_trait_gaps <- function(traits) {
  comrad::testarg_num(traits)
  if (any(traits != sort(traits))) {
    stop("'traits' must be sorted by ascending order before checking for gaps.")
  }

  trait_dist <- traits %>%
    diff() %>%
    abs() %>%
    round(digits = 2) # 1st circle of hell: trusting floating points in R

  gaps <- which(trait_dist >= 0.1)
  gaps
}
