#' Average AICw scores over all replicate trees
#'
#' Only relevant if DD models have been fit separately on each tree, i.e. the
#' input `aicw_tbl` must contain a variable `tree`.
#'
#' @param aicw_tbl a data frame with AICw scores for each model and tree
#'
#' @export
summarise_aicw_over_trees <- function(aicw_tbl) {

  trees_to_exclude <- aicw_tbl %>%
    dplyr::filter(loglik == -Inf) %>%
    dplyr::pull(tree) %>%
    unique()

  aicw_tbl %>%
    dplyr::filter(!tree %in% trees_to_exclude) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dd_model) %>%
    dplyr::summarise(
      "n" = dplyr::n(),
      "aicw" = sum(aicw) / n
    ) %>%
    dplyr::select(dd_model, aicw)
}
