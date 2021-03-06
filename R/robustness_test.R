#' Run a robustness test as a measure of absolute fit
#'
#' This function will run a test that is meant as a measure of the absolute fit
#' of a DD model to a set of comrad trees. The test uses the delta nLTT statistic
#' (Janzen et al. 2015) as a measure of error between the branching pattern expected under a
#' DD model and that observed in a set of comrad trees. Inspired from the bootstrap tests
#' of Louca & Pennell (2021) and Santos Neves, Lambert et al. (2021). Deterministic LTT of the DD
#' model estimated as the average LTT of the bootstrap set.
#'
#' Here, "empirical trees" is meant as a set of trees to which the fit of a DD model is to be assessed,
#' while "bootstrap trees" are a set of trees simulated under the DD model, with maximum likelihood estimates for the
#' set of empirical trees.
#'
#' @param ltts_empirical a list of `tibble` with columns `time` and `N` (as produced by [get_ltt_tbl()])
#' containing the LTTs of the set of empirical trees.
#' @param ltts_bootstrap a list of `tibble` with columns `time` and `N` (as produced by [get_ltt_tbl()])
#' containing the LTTs of the set of bootstrap trees.
#'
#' @return a list with four elements:
#' - `dnltts_empirical` vector with the delta nLTT between every tree in the empirical set and the average bootstrap LTT
#' - `dnltts_bootstrap` vector with the delta nLTT between every tree in the bootsrap set and the average bootstrap LTT
#' - `p_95` a measure akin a p-value, the proportion of empirical trees with a delta nLTT higher than the 95th percentile of the distribution delta nLTT of the bootstrap test.
#' Based on the $p_95$ of Santos Neves, Lambert et al. (2021).
#' - `ks` output of a two-sample, one-sided (empirical > bootstrap) Kolmogorv-Smirnov test on the distribution of the delta nLTT of the empirical and bootsrap test.
#'
#' @export
#' @author Theo Pannetier

robustness_test <- function(ltts_empirical, ltts_bootstrap) {

  # Compute average LTT of the bootstrap test, estimate of the deterministic LTT of the model
  t_seq <- seq(min(ltts_empirical[[1]]$time), 0, 100)
  avg_ltt_bootstrap <- ltts_bootstrap %>%
    average_ltt(t_seq)

  # Compute delta nLTT vs reference LTT for the trees of each set
  dnltts_bootstrap <- ltts_bootstrap %>% purrr::map_dbl(get_dnltt, avg_ltt_bootstrap)
  dnltts_empirical <- ltts_empirical %>% purrr::map_dbl(get_dnltt, avg_ltt_bootstrap)

  # Get decision criteria
  dnltt_95th <- stats::quantile(dnltts_bootstrap, probs = 0.95)
  p_95 <- 1 - sum(dnltts_empirical > dnltt_95th) / length(dnltts_empirical)
  ks <- stats::ks.test(dnltts_empirical, dnltts_bootstrap, alternative = "less")

  return(list(
    "dnltts_empirical" = dnltts_empirical,
    "dnltts_bootstrap" = dnltts_bootstrap,
    "p_95" = p_95,
    "ks" = ks
  ))
}
