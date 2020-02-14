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
  expect_error(
    read_comrad_tbl(1313),
    "'path_to_file' must be a character."
  )
  expect_error(
    read_comrad_tbl(character(0)),
    "'path_to_file' is empty"
  )
  expect_error(
    read_comrad_tbl("notacsv"),
    "'path_to_file' must be a .csv"
  )

  # Plots
  expect_true(
    plot_pop_trait_evolution(comrad_tbl) %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_pop_trait_evolution(comrad_tbl, generation_range = c(0,10)),
    "generation_range is out of the scope of generations in the comrad_tbl."
  )
  expect_true(
    plot_generation_traits(
      comrad_tbl, generation = 1
    ) %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_generation_traits(comrad_tbl, generation = 100),
    "Generation 100 wasn't sampled."
  )
  expect_true(
    plot_population_size(comrad_tbl) %>%
      ggplot2::is.ggplot()
  )

  # Test phylogeny
  # not a legit phylogeny (1 tip), but the beam though
  phylo_tbl <- comrad_tbl %>% comrad:::assemble_phylo_tbl()
  expect_equal(
    phylo_tbl,
    tibble::tibble(
      "species_name" = "#89ae8a",
      "ancestor_name" = as.character(NA),
      "time_birth" = 0,
      "time_death" = 5
    )
  )
  expect_equal(
    phylo_tbl %>% comrad::convert_to_newick(),
    "#89ae8a:5;"
  )
  unlink(temp_output_path)
})

test_that("phylogeny_hoaxids", {
  # Now with a proper phylogeny
  phylo_tbl_hoaxids <- tibble::tibble(
    "species_name" = c(
      "Haggis_scoticus", "Dahu_senestris", "Dahu_dextris", "Thylarctos_plummetus"
    ),
    "ancestor_name" = c(
      as.character(NA), "Haggis_scoticus", "Dahu_senestris", "Haggis_scoticus"
    ),
    "time_birth" = c(0, 1, 2, 2),
    "time_death" = c(5, 5, 5, 5)
  )
  expect_silent(
    newick_str <- phylo_tbl_hoaxids %>% comrad::convert_to_newick()
  )
  # can't test plots without creating unwanted Rplots.pdf in /testthat
})
