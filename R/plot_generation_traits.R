#' Plot trait distribution in a generation
#'
#' Plots a histogram of the distribution of trait values in a single generation.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param generation numeric, the index of the generation to plot
#' @param binwidth numeric, the width used for binning.
#'
#' @author Théo Pannetier
#' @export

plot_generation_traits <- function(comrad_tbl,
                                  generation,
                                  binwidth = 0.02) {
  testarg_num(generation)
  testarg_pos(generation)
  testarg_num(binwidth)
  testarg_pos(binwidth)

  # Stupid but necessary for the build
  z <- NULL
  ..x.. <- NULL

  if (!generation %in% comrad_tbl$t) {
    stop(paste("Generation", generation, "wasn't sampled."))
  }

  max_gen <- max(comrad_tbl$t)

  gen_data <- comrad_tbl %>% dplyr::filter(comrad_tbl$t == generation)
  gen_data %>%
    ggplot2::ggplot(ggplot2::aes(x = z, fill = ..x..)) +
    ggplot2::geom_histogram(binwidth = binwidth) +
    ggplot2::scale_fill_gradient2(
      low = "blue", mid = "blue", high = "blue", name = "Trait"
      ) +
    ggplot2::labs(
      x = "Trait",
      y = "Count",
      title = paste("Generation", generation, "/", max_gen)
    )
}
