#' Plot population size over generations
#'
#' Plots the number of individuals in the population over time.
#'
#' @inheritParams default_params_doc
#'
#' @author Théo Pannetier
#' @export

plot_population_size <- function(sim_tbl) {
  test_sim_tbl(sim_tbl)

  growth_plot <- sim_tbl %>%
    dplyr::group_by(t) %>%
    dplyr::count() %>%
    ggplot2::ggplot(ggplot2::aes(x = sim_tbl$t, y = sim_tbl$n)) +
    ggplot2::geom_area(color = "skyblue4", fill = "skyblue4") +
    ggplot2::geom_hline(yintercept = 1000, linetype = "dashed", color = "grey60") +
    ggplot2::labs(x = "Generation", y = "Nb of individuals")
  growth_plot
}
