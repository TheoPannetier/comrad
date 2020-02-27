#' Plot the evolution of trait values in the community over generations
#'
#' Produces a hex-density plot of all trait values in the community over
#' generations.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param generation_range numeric vector with length 2, supplying the first and
#' last generation to plot from the dataset. Defaults to every generation in the
#' table
#' @param xgrain numeric, the width of  hexes on the x-axis (generations)
#' @param ygrain numeric, the width of  hexes on the y-axis (trait)
#' @param hex_fill character with two options, `"counts"` and `"species"`.
#' `"counts"` colours hexes by the count of individuals, and `"species"` by
#' species identity.
#'
#' @author Théo Pannetier
#' @export


plot_comm_trait_evolution <- function(comrad_tbl,
                                      generation_range = c(0, Inf),
                                      xgrain = 100,
                                      ygrain = 0.01,
                                      hex_fill = "counts") {
  comrad::testarg_num(generation_range)
  comrad::testarg_pos(generation_range)
  comrad::testarg_length(generation_range, 2)
  comrad::testarg_num(xgrain)
  comrad::testarg_pos(xgrain)
  comrad::testarg_num(ygrain)
  comrad::testarg_pos(ygrain)

  if (!hex_fill %in% c("counts", "species")) {
    stop("'hex_fill' must be either 'counts' or 'species' (see doc).")
  }

  # Stupid but necessary for the build
  z <- NULL
  t <- NULL
  species <- NULL

  if (generation_range[2] == Inf) {
    generation_range[2] <- max(comrad_tbl$t)
  }
  if (any(!(generation_range %in% comrad_tbl$t))) {
    stop(
      "generation_range is out of the scope of generations in the comrad_tbl."
    )
  }
  species_names <- unique(comrad_tbl$species)
  names(species_names) <- species_names

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

  trait_plot <- comrad_tbl %>%
    dplyr::filter(
      dplyr::between(t, generation_range[1], generation_range[2])
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = z)) +
    ggplot2::scale_y_continuous(minor_breaks = seq(-2, 2, 0.1)) +
    ggplot2::labs(x = "Generation", y = "Trait")

  if (hex_fill == "species") {
    trait_plot <- trait_plot +
      ggplot2::geom_hex(
        ggplot2::aes(fill = species, alpha = 0.8),
        binwidth = c(xgrain, ygrain),
        show.legend = FALSE
      ) +
      ggplot2::scale_colour_manual(
        values = species_names,
        aesthetics = c("fill")
      )
  } else {
    trait_plot <- trait_plot +
      ggplot2::geom_hex(
        binwidth = c(xgrain, ygrain),
        show.legend = FALSE
      ) +
      viridis::scale_fill_viridis(option = "B")
  }
  trait_plot
}
