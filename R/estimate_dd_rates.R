#' Estimate diversity-dependent rates from phylogenies
#'
#' For a list of phylogenies, reconstruct the *average, per-capita* rates of
#' speciation and extinction corresponding to each level of species diversity.
#'
#' @param multi_phylo a list of `phylo` objects, as typically created with
#' `ape`.
#' @param pool_events logical, pool speciation and extinction events together
#' before computing the rates?
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

estimate_dd_rates <- function(multi_phylo, pool_events = TRUE) {

  # no NOTE
  # nolint start
  N <- NULL
  next_event <- NULL
  n_events <- NULL
  p_event <- NULL
  waiting_time <- NULL
  mean_waiting_time <- NULL
  event_rate <- NULL
  p_speciation <- NULL
  p_extinction <- NULL
  # nolint end

  # Extract waiting times for all phylos
  times_tbl <- multi_phylo %>%
    purrr::map_dfr(waiting_times, pool_events = pool_events, .id = "replicate")

  if (pool_events) {
    # Prop of speciation/extinction events in each N bin
    p_tbl <- times_tbl %>%
      dplyr::group_by(N, next_event) %>%
      dplyr::summarise(
        "n_events" = dplyr::n()
      ) %>%
      dplyr::mutate(
        "p_event" = n_events / sum(n_events)
      ) %>%
      tidyr::pivot_wider(
        id_cols = N,
        names_from = next_event,
        names_glue = "p_{next_event}",
        values_from = p_event
      )
    # Special fix if there is zero extinction event
    if (sum(times_tbl$next_event == "extinction") == 0) {
      p_tbl <- p_tbl %>% dplyr::mutate("p_extinction" = NA)
    }

    # Compute rates pooled as (spec_rate + ext_rate)
    rates_tbl <- times_tbl %>%
      dplyr::group_by(N) %>%
      dplyr::summarise(
        "mean_waiting_time" = mean(waiting_time, na.rm = TRUE)
      ) %>%
      # In case all trees are polytomies (mean wt = 0), drop this point
      dplyr::mutate(
        "mean_waiting_time" = ifelse(
          mean_waiting_time == 0, NA, mean_waiting_time
        )
      ) %>%
    dplyr::mutate(
      "event_rate" = 1 / (mean_waiting_time * N)
    ) %>%
      # Separate rates by proportion of each event in each bin
      dplyr::left_join(p_tbl, by = "N") %>%
      dplyr::mutate(
        "speciation_rate" = event_rate * p_speciation,
        "extinction_rate" = event_rate * p_extinction
      )
  } else {
    # Compute rates separately
    rates_tbl <- times_tbl %>%
      dplyr::group_by(N, next_event) %>%
      dplyr::summarise(
        "mean_waiting_time" = mean(waiting_time, na.rm = TRUE)
      ) %>%
      # In case all trees are polytomies (mean wt = 0), drop this point
      dplyr::mutate(
        "mean_waiting_time" = ifelse(
          mean_waiting_time == 0, NA, mean_waiting_time
        )
      ) %>%
      dplyr::mutate(
        "event_rate" = 1 / (mean_waiting_time * N)
      ) %>%
      tidyr::pivot_wider(
        id_cols = N,
        names_from = next_event,
        names_glue = "{next_event}_rate",
        values_from = event_rate
      )
  }

  rates_tbl <- rates_tbl %>%
    dplyr::select(N, speciation_rate, extinction_rate)

  return(rates_tbl)
}
