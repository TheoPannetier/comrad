#' Plot the evolution of trait values in the population over generations
#'
#' Reads a simulation data table and produces a hex-density plot of all trait
#' values in the population over generations.
#'
#' @param path_to_file character, path to a `.csv` file as produced by
#' [run_simulation()]
#' @param xgrain numeric, the width of  hexes on the x-axis (generations)
#' @param ygrain numeric, the width of  hexes on the y-axis (trait)
#'
#' @author Théo Pannetier
#' @export

plot_population_trait_evolution <- function(path_to_file,
                                 xgrain = 10,
                                 ygrain = 0.01) {
  testarg_char(path_to_file)
  testarg_num(xgrain)
  testarg_pos(xgrain)
  testarg_num(ygrain)
  testarg_pos(ygrain)

  data <- readr::read_csv(
    path_to_file,
    skip = 14)

  # rm last row
  data <- data[-length(data[[1]]), ]
  tail(data, 5)

  # xgrain <- max(data$t) / 100

  trait_plot <- data %>% ggplot2::ggplot(ggplot2::aes(x = t, y = z)) +
    ggplot2::geom_hex(binwidth = c(xgrain, ygrain)) +
    viridis::scale_fill_viridis(option = "B") +
    ggplot2::labs(x = "Generation", y = "Trait")
  trait_plot
}
