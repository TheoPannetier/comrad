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

  # Test phylogeny
  # not a legit phylogeny (1 tip) but the beam though
  phylo_tbl <- comrad_tbl %>% comrad::assemble_phylo_tbl()
  expect_equal(
    phylo_tbl,
    tibble::tibble(
      "species_name" = "Haggis_scoticus",
      "ancestor_name" = as.character(NA),
      "time_birth" = 0,
      "time_death" = 5
    )
  )
  expect_equal(
    phylo_tbl %>% comrad::convert_to_newick(),
    "Haggis_scoticus:5;"
  )

  unlink(temp_output_path)
})
