#' Plot trait distribution in a generation
#'
#' Plots a histogram of the distribution of trait values in a single generation.
#'
#' @inheritParams default_params_doc
#' @param generation numeric, the index of the generation to plot
#' @param binwidth numeric, the width used for binning.
#'
#' @author Théo Pannetier
#' @export

plot_generation_traits <- function(sim_tbl,
                                  generation,
                                  binwidth = 0.02) {
  test_sim_tbl(sim_tbl)
  testarg_num(generation)
  testarg_pos(generation)
  testarg_num(binwidth)
  testarg_pos(binwidth)

  if (!generation %in% sim_tbl$t) {
    stop(paste("Generation", generation, "wasn't sampled."))
  }

  max_gen <- max(sim_tbl$t)

  gen_data <- sim_tbl %>% dplyr::filter(sim_tbl$t == generation)
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
