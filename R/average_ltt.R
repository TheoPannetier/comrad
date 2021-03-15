#' Compute the average LTT for a set of trees given a time sequence
#'
#' @param ltt_tbls a list of `tibble` that contain the LTT of the trees with columns
#' `time` and `N`, e.g. the output of [get_ltt_tbl()].
#' @param t_seq a vector of doubles containing the times at which the average N
#' should be evaluated.
#'
#' @return a `tibble` with columns `time` and `N`, the average number of lineages
#' in the set along `t_seq`
#' @author Theo Pannetier
#' @export
#'
average_ltt <- function(ltt_tbls, t_seq) {
  avg_ltt_tbl <- ltt_tbls %>% purrr::map_dfr(interpolate_ltt, t_seq) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise("N" = mean(N), .groups = "drop")
  return(avg_ltt_tbl)
}
