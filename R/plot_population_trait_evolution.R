#' Plot the evolution of trait values in the population over generations
#'
#' Reads a simulation data table and produces a hex-density plot of all trait
#' values in the population over generations.
#'
#' @param path_to_file character, path to a `.csv` file as produced by
#' [run_simulation()]
#' @param generation_range numeric vector with length 2, supplying the first and
#' last generation to plot from the dataset. Defaults to very generation in the
#' table
#' @param xgrain numeric, the width of  hexes on the x-axis (generations)
#' @param ygrain numeric, the width of  hexes on the y-axis (trait)
#'
#' @author Théo Pannetier
#' @export

plot_population_trait_evolution <- function(path_to_file,
                                            generation_range = c(0, Inf),
                                            xgrain = 10,
                                            ygrain = 0.01) {
  testarg_char(path_to_file)
  testarg_num(generation_range)
  testarg_pos(generation_range)
  testarg_length(generation_range, 2)
  testarg_num(xgrain)
  testarg_pos(xgrain)
  testarg_num(ygrain)
  testarg_pos(ygrain)

  data <- readr::read_csv(
    path_to_file,
    skip = 15)

  # rm last row
  data <- data[-length(data[[1]]), ]
  utils::tail(data, 5)

  if (generation_range[2] == Inf) {
    generation_range[2] <- max(data$t)
  }
  if (any(!(generation_range %in% data$t))) {
    stop("generation_range is out of the scope of generations in the data.")
  }

  trait_plot <- data %>%
    dplyr::filter(
      dplyr::between(t, generation_range[1], generation_range[2])
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = z)) +
    ggplot2::geom_hex(binwidth = c(xgrain, ygrain)) +
    viridis::scale_fill_viridis(option = "B") +
    ggplot2::labs(x = "Generation", y = "Trait")
  trait_plot
}
