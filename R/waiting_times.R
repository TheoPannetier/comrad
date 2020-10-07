#' Extract times to events from a phylogeny
#'
#' Turns the phylogeny into a lineage-through-time table, and for which
#' event computes the time to next event.
#'
#' @param phylo an object of the class `phylo` as introduced in
#' [ape][ape::read.tree]. The phylogeny must start with the crown node
#' (not stem), and be binary (no hard polytomies).#'
#' @return a `tibble` with a row per event and four columns:
#'
#' * `time`, time of an event
#' * `N`, number of species in the tree at that time
#' * `next_event`, type of the next event; either `"speciation"` or
#' `"extinction"`
#' * `waiting_time`, time to the next event
#'
#' @author Théo Pannetier
#' @export

waiting_times <- function(phylo) {

  if (!class(phylo) == "phylo") {
    stop("'phylo' must be a 'phylo' object.")
  }
  time <- NULL # no NOTE

  # Get time and N from phylobates
  ltt_tbl <- phylo %>%
    get_ltt_tbl() %>%
    dplyr::mutate("time" = time - min(time))
  # Drop last row (present)
  ltt_tbl <- ltt_tbl[-nrow(ltt_tbl), ]

  wt_tbl <- ltt_tbl %>%
    # What is next event ?
    dplyr::mutate(
      "next_event" = ifelse(
        test = dplyr::lead(ltt_tbl$N) - ltt_tbl$N > 0,
        yes = "speciation",
        no = "extinction"
      ),
      # Time to next event
      "waiting_time" = dplyr::lead(time) - time
    )
  # Drop last row, waiting time cut by present (NA)
  wt_tbl <- wt_tbl[-nrow(wt_tbl), ]

  return(wt_tbl)
}
