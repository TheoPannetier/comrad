#' Extract times to events from a phylogeny
#'
#' Turns the phylogeny into a lineage-through-time table, and for which
#' event computes the time to next event.
#'
#' @param phylo an object of the class `phylo` as introduced in
#' [ape][ape::read.tree]. The phylogeny must start with the crown node
#' (not stem), and be binary (no hard polytomies).#'
#' @param pool_events logical, do you want the waiting time to next event
#' (regardless of speciation, extinction), or next event of same type?
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

waiting_times <- function(phylo, pool_events = TRUE) {

  if (!class(phylo) == "phylo") {
    stop("'phylo' must be a 'phylo' object.")
  }

  # nolint start
  next_event <- NULL # no NOTE
  time <- NULL # no NOTE
  # nolint end

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
      )
    )

  # Waiting time to...
  wt_tbl <- if (pool_events) {
    # any next event
    wt_tbl %>%
      dplyr::mutate("waiting_time" = dplyr::lead(time) - time)
  } else {
    # event of same type
    wt_tbl %>% dplyr::group_by(next_event) %>%
      dplyr::mutate("waiting_time" = dplyr::lead(time) - time)
  }

  # Drop last row, waiting time cut by present (NA)
  wt_tbl <- wt_tbl[-nrow(wt_tbl), ]

  return(wt_tbl)
}
