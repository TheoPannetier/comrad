#' Simulate a population undergoing brownian mutations
#'
#' This function serves to test the mutation rate in the main simulation. Runs
#' and plots trait values undergoing brownian motion under the specified
#' mutation rate.
#'
#' @inheritParams default_params_doc
#' @param pop_size integer, size of the population to simulate.
#' @param nb_gen integer, number of generations to run the simulation for.
#'
#' @author Théo Pannetier

simulate_brownian_mutation <- function(pop_size = 1000,
                                  nb_gen = 1000,
                                  mutation_sd = default_mutation_sd()) {
  # Initial pop
  pop_traits <- rep(0, pop_size)
  pop_tbl <- dplyr::tibble(
    "trait" = pop_traits,
    "gen" = 0,
    "id" = 1:pop_size
    )
  # Run simulation
  for (gen in 1:nb_gen) {
    pop_traits <- pop_traits + stats::rnorm(pop_size, 0, mutation_sd)
    gen_tbl <- dplyr::tibble(
      "trait" = pop_traits,
      "gen" = gen,
      "id" = 1:pop_size
      )
    pop_tbl <- rbind(pop_tbl, gen_tbl)
  }
  # Plot results
  gg <- ggplot2::ggplot(pop_tbl, ggplot2::aes(
    x  = pop_tbl$gen,
    y = pop_tbl$trait,
    group = pop_tbl$id
    )) +
    ggplot2::geom_line(alpha = 0.02)

}



