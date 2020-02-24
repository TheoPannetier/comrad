#' Plot the evolution of trait values in the population over generations
#'
#' Produces a hex-density plot of all trait values in the population over
#' generations.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param generation_range numeric vector with length 2, supplying the first and
#' last generation to plot from the dataset. Defaults to every generation in the
#' table
#' @param xgrain numeric, the width of  hexes on the x-axis (generations)
#' @param ygrain numeric, the width of  hexes on the y-axis (trait)
#'
#' @author Théo Pannetier
#' @export


plot_pop_trait_evolution <- function(comrad_tbl,
                                            generation_range = c(0, Inf),
                                            xgrain = 10,
                                            ygrain = 0.01) {
  comrad::testarg_num(generation_range)
  comrad::testarg_pos(generation_range)
  comrad::testarg_length(generation_range, 2)
  comrad::testarg_num(xgrain)
  comrad::testarg_pos(xgrain)
  comrad::testarg_num(ygrain)
  comrad::testarg_pos(ygrain)

  # Stupid but necessary for the build
  z <- NULL
  t <- NULL

  if (generation_range[2] == Inf) {
    generation_range[2] <- max(comrad_tbl$t)
  }
  if (any(!(generation_range %in% comrad_tbl$t))) {
    stop(
      "generation_range is out of the scope of generations in the comrad_tbl."
      )
  }

  trait_plot <- comrad_tbl %>%
    dplyr::filter(
      dplyr::between(t, generation_range[1], generation_range[2])
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = z)) +
    ggplot2::geom_hex(binwidth = c(xgrain, ygrain)) +
    viridis::scale_fill_viridis(option = "B") +
    ggplot2::scale_y_continuous(minor_breaks = seq(-2, 2, 0.1)) +
    ggplot2::labs(x = "Generation", y = "Trait")
  trait_plot
}
