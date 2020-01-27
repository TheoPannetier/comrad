#' Plot trait distribution in a generation
#'
#' Reads a simulation data table and plots a histogram of the distribution of
#' trait values in a single generation.
#'
#' @param path_to_file character, path to a `.csv` file as produced by
#' [run_simulation()]
#' @param generation numeric, the index of the generation to plot
#' @param binwidth numeric, the width used for binning.
#'
#' @author Théo Pannetier
#' @export

plot_generation_traits<- function(path_to_file,
                                  generation,
                                  binwidth = 0.02) {
  testarg_char(path_to_file)
  testarg_num(generation)
  testarg_pos(generation)
  testarg_num(binwidth)
  testarg_pos(binwidth)

  data <- readr::read_csv(
    path_to_file,
    skip = 15)
  # rm last row
  data <- data[-length(data[[1]]), ]
  utils::tail(data, 5)

  if (!generation %in% data$t) {
    stop(paste("Generation", generation, "wasn't sampled."))
  }

  max_gen <- max(data$t)

  gen_data <- data %>% dplyr::filter(data$t == generation)
  gen_data %>%
    ggplot2::ggplot(ggplot2::aes(x = gen_data$z, fill = ..x..)) +
    ggplot2::geom_histogram(binwidth = binwidth) +
    ggplot2::scale_fill_gradient2(
      low = "blue", mid = "red", high = "blue", name = "Trait"
      ) +
    ggplot2::labs(
      x = "Trait",
      y = "Count",
      title = paste("Generation", generation, "/", max_gen)
    )
}
