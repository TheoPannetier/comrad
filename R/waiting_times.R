#' Extract times to events from a phylogeny
#'
#' Turns the phylogeny into a lineage-through-time table, and for which
#' event computes the time to next event.
#'
#' @param phylo a `phylo` object, as created by `ape` functions
#'
#' @return a `tibble` with a row per event and four columns:
#'
#' * `time`, time of an event
#' * `N`, number of species in the tree at that time
#' * `waiting_time`, time to the next event
#' * `next_event`, type of the next event; either `"speciation"` or
#' `"extinction"`
#'
#' @author Théo Pannetier
#' @export

waiting_times <- function(phylo) {
  if (!ape::is.binary.phylo(phylo)) {
    stop("'phylo' must be a 'phylo' object.")
  }

  time <- NULL

  # Get time and N from ape
  ltt_tbl <- phylo %>%
    ape::ltt.plot.coords() %>%
    tibble::as_tibble()

  ltt_tbl <- ltt_tbl %>% dplyr::mutate(
    waiting_time = time - dplyr::lag(time)
  ) %>%
    dplyr::mutate(
      # ape misplaces time relative to N, fix that
      time = dplyr::lag(time - min(time))
    )
  # first row is useless
  ltt_tbl <- ltt_tbl[-1, ]

  # Time to speciation or extinction ?
  ltt_tbl <- ltt_tbl %>%
    dplyr::mutate(
      "event" = ifelse(
        test = dplyr::lead(ltt_tbl$N) - ltt_tbl$N > 0,
        yes = "speciation",
        no = "extinction"
      )
    )
  # exclude last row, waiting time is cut by present
  ltt_tbl <- ltt_tbl[-nrow(ltt_tbl), ]


  ltt_tbl
}
