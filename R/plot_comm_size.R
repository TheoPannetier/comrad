#' Plot community size over generations
#'
#' Plots the number of individuals in the community over time.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param colouring character specifying how to colour each species. Default
#' `auto` lets `ggplot` pick colours as usual. `species_names` is used when
#' colour hexes are sampled as species names during the simulation, and will
#' colour each species with its colour. This ensures consistency across plots
#' but may make colours harder to distinguish based on which hexes where
#' sampled.
#'
#' @author Théo Pannetier
#' @export

plot_comm_size <- function(comrad_tbl, colouring = "auto") {
  if (!colouring %in% c("auto", "species_names")) {
    stop("'colouring' must be either 'auto' or 'species_names' (see doc).")
  }
  # Stupid but necessary for the build
  n <- NULL
  t <- NULL
  species <- NULL

  # Extract species names for colours
  species_names <- unique(comrad_tbl$species)
  names(species_names) <- species_names

  comrad_tbl$species <- factor(comrad_tbl$species, levels = species_names)

  growth_plot <- comrad_tbl %>%
    dplyr::group_by(t, species) %>%
    dplyr::count() %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = n, fill = species)) +
    ggplot2::geom_area(show.legend = FALSE) +
    ggplot2::labs(x = "Generation", y = "Nb of individuals")
  if (colouring == "species_names") {
    growth_plot <- growth_plot +
      ggplot2::scale_fill_manual(values = species_names)
  }
  growth_plot
 }
