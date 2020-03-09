#' Find gaps in trait values
#'
#' Runs through an ordered vector of trait values, returns the positions of gaps
#' `>= trait_gap` between consecutive values.
#'
#' @param traits a numeric vector, trait values **in ascending order**.
#' @param trait_gap numeric, the width of a gap triggering speciation.
#'
#' @author Théo Pannetier
#' @export

find_trait_gaps <- function(traits, trait_gap = 0.1) {
  comrad::testarg_num(traits)
  if (any(traits != sort(traits))) {
    stop("'traits' must be sorted by ascending order before checking for gaps.")
  }

  trait_dist <- traits %>%
    diff() %>%
    abs() %>%
    round(digits = 2) # 1st circle of hell: trusting floating points in R

  gaps <- which(trait_dist >= trait_gap)
  gaps
}
