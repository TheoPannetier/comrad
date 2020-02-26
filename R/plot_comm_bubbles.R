#' Plot population densities and trait distribution over time, with bubbles
#'
#' Produces a plot of trait evolution where individuals are binned as bubbles
#' which colour indicate the species identity and the size the number of
#' individuals in the bin. Equivalent to [plot_comm_trait_evolution()], plus
#' species identities.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param generation_range numeric vector with length 2, supplying the first and
#' last generation to plot from the dataset. Defaults to every generation in the
#' table
#'
#' @author Théo Pannetier
#' @export

plot_comm_bubbles <- function(comrad_tbl,
                              generation_range = c(0, Inf)
) {
  comrad::testarg_num(generation_range)
  comrad::testarg_pos(generation_range)
  comrad::testarg_length(generation_range, 2)

  # Stupid but necessary for the build
  z <- NULL
  t <- NULL
  species <- NULL
  count <- NULL

  if (generation_range[2] == Inf) {
    generation_range[2] <- max(comrad_tbl$t)
  }
  if (any(!(generation_range %in% comrad_tbl$t))) {
    stop(
      "generation_range is out of the scope of generations in the comrad_tbl."
    )
  }
  comrad_tbl <- comrad_tbl %>%
    dplyr::filter(
      dplyr::between(t, generation_range[1], generation_range[2])
    )
  species_hexes <- unique(comrad_tbl$species)
  names(species_hexes) <- species_hexes

  trait_plot <- comrad_tbl %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = z)) +
    ggplot2::stat_bin_2d(
      geom = "point",
      ggplot2::aes(
        size = stat(count),
        colour = species,
        alpha = 0.5
      ),
      show.legend = c(
        "size" = TRUE, "colour" = FALSE, "count" = FALSE, "alpha" = FALSE
      )
    ) +
    ggplot2::scale_colour_manual(values = species_hexes) +
    ggplot2::scale_y_continuous(minor_breaks = seq(-2, 2, 0.1)) +
    ggplot2::labs(x = "Generation", y = "Trait value")

  trait_plot
}
