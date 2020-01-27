#' Plot population size over generations
#'
#' Reads a simulation data table and plots the number of individuals in the
#' population over time
#'
#' @param path_to_file character, path to a `.csv` file as produced by
#' [run_simulation()]
#'
#' @author Théo Pannetier
#' @export

plot_population_size <- function(path_to_file) {
  testarg_char(path_to_file)

  data <- readr::read_csv(
    path_to_file,
    skip = 15)

  # rm last row
  data <- data[-length(data[[1]]), ]
  utils::tail(data, 5)

  # xgrain <- max(data$t) / 100

  growth_plot <- data %>%
    dplyr::group_by(t) %>%
    dplyr::count() %>%
    ggplot2::ggplot(ggplot2::aes(x = data$t, y = data$n)) +
    ggplot2::geom_area(color = "skyblue4", fill = "skyblue4") +
    ggplot2::geom_hline(yintercept = 1000, linetype = "dashed", color = "grey60") +
    ggplot2::labs(x = "Generation", y = "Nb of individuals")
  growth_plot
}
