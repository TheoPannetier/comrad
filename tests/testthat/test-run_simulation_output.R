context("test-run_simulation_output")

test_that("standard_output_file", {
  temp_output_path <- paste0(tempfile("comrad_test_output"), ".csv")
  run_simulation(output_path = temp_output_path, nb_generations = 5)

  # Comrad table is standard
  expect_silent(comrad_tbl <- read_comrad_tbl(temp_output_path))
  expect_silent(
    comrad_tbl %>%
      dplyr::select("z", "species", "ancestral_species") %>%
      comrad::test_comrad_pop()
  )

  # Plots
  expect_true(
    plot_pop_trait_evolution(comrad_tbl) %>%
      ggplot2::is.ggplot()
  )
  expect_true(
    plot_generation_traits(
      comrad_tbl, generation = 1
    ) %>%
      ggplot2::is.ggplot()
  )
  expect_true(
    plot_population_size(comrad_tbl) %>%
      ggplot2::is.ggplot()
  )

  unlink(temp_output_path)
})
