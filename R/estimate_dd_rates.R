#' Estimate diversity-dependent rates from phylogenies
#'
#' For a list of phylogenies, reconstruct the *average, per-capita* rates of
#' speciation and extinction corresponding to each level of species diversity.
#'
#' @param multi_phylo a list of `phylo` objects, as typically created with
#' `ape`.
#'
#' @return a `tibble` with a row for every possible number of species and 4
#' columns:
#'
#' * `N`, level of diversity, nb of species
#' * `nb_events`, the number of replicates for that level of diversity; that is,
#' the number of events corresponding to that state of diversity, across all
#' phylogenies in `multi_phylo`.
#' * `speciation_rate` the reconstructed, average, per-capita speciation rate
#' for that level of diversity
#' * `extinction_rate` the reconstructed, average, per-capita extinction rate
#' for that level of diversity
#'
#' @return
#' @author Théo Pannetier
#' @export
#'

estimate_dd_rates <- function(multi_phylo) {

  if (!is.list(multi_phylo)) {
    stop("'multi_phylo' must be a list of 'phylo' objects.")
  } else if (!all(ape::is.binary.multiPhylo(multi_phylo))) {
    stop("'multi_phylo' must be a list of 'phylo' objects.")
  }

  # Declare empty variables for check
  N <- NULL # nolint
  time <- NULL #nolint
  event_rate <- NULL #nolint
  waiting_time <- NULL # nolint
  prop_speciation <- NULL # nolint
  prop_extinction <- NULL # nolint
  speciation_rate <- NULL # nolint
  extinction_rate <- NULL # nolint
  mean_waiting_time <- NULL # nolint
  nb_events <- NULL # nolint
  event <- NULL # nolint
  total_events <- NULL # nolint

  replicates <- seq_along(multi_phylo)

  # Extract waiting times for all phylos
  times_tbl <- lapply(replicates, function(i) {
    phylo <- multi_phylo[[i]]
    waiting_times(phylo) %>%
      dplyr::mutate("replicate" = i)
  }) %>%
    dplyr::bind_rows()

  # Get nb of events (speciation + extinction) for each N
  sample_sizes <- times_tbl %>%
    dplyr::group_by(N) %>%
    dplyr::summarise(
      "total_events" = dplyr::n()
    )
  # Average wating times
  rates_tbl <- times_tbl %>%
    dplyr::group_by(N, event) %>%
    dplyr::summarise(
      "mean_waiting_time" = mean(waiting_time, na.rm = TRUE),
      "nb_events" = dplyr::n()
    )
  # Compute speciation & extinction rates
  rates_tbl <- rates_tbl %>%
    dplyr::bind_cols(sample_sizes[rates_tbl$N, "total_events"]) %>%
    dplyr::mutate(
      "event_rate" = (1 / mean_waiting_time) * (1 / N) *
        (nb_events / total_events)
    )
  rates_tbl
}
