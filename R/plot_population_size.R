#' Plot population size over generations
#'
#' Plots the number of individuals in the population over time.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#'
#' @author Théo Pannetier
#' @export

plot_population_size <- function(comrad_tbl) {

  growth_plot <- comrad_tbl %>%
    dplyr::group_by(t) %>%
    dplyr::count() %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = n)) +
    ggplot2::geom_area(color = "skyblue4", fill = "skyblue4") +
    ggplot2::labs(x = "Generation", y = "Nb of individuals")
  growth_plot
}
