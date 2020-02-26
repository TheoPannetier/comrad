context("test-run_simulation")

test_that("diverging_community", {
  # community starts with split values, expect speciation in the next step
  diverging_pop <- default_init_comm()
  diverging_pop$z[6:10] <- 0.1
  output <- run_simulation(
    init_comm = diverging_pop, nb_generations = 1, output_path = NULL
  )
  species <- output %>%
    dplyr::filter(t == 1) %>%
    dplyr::select(species) %>%
    unique %>%
    unlist()
  ancestral_species <- output %>%
    dplyr::filter(t == 1) %>%
    dplyr::select(ancestral_species) %>%
    unique() %>%
    unlist()
  expect_gt(length(species), 1)
  expect_gt(length(ancestral_species), 1)
})

test_that("output_format", {
  nb_generations <- 5
  output <- run_simulation(nb_generations = nb_generations, output_path = NULL)

  expect_equal(length(output), 5)
  # Main community columns
  expect_silent(
    output[, c("z", "species", "ancestral_species")] %>% test_comrad_comm()
  )
  # Format of columns missed by test_comrad_comm()
  expect_true(any(output$t >= 0))
  expect_true(any(output$runtime >= 0))
  # Assert output corresponds to last generation
  expect_equal(unique(output$t), nb_generations)
})

test_that("extinction", {
  expect_output(
    # TPK
    output <- run_simulation(
      carr_cap_opt = 0, nb_generations = 1, output_path = NULL),
    "\\nRunning generation 1 / 1\\nCommunity has gone extinct at generation 1 "
  )
})

test_that("parameter_abuse", {
  expect_error(
    run_simulation(init_comm = rep(0, 10), output_path = NULL),
    "'init_comm' should be a tibble."
  )
  expect_error(
    run_simulation(output_path = 1),
    "'output_path' must be either null or a character."
  )
  expect_error(
    run_simulation(nb_generations = 12.3, output_path = NULL),
    "'nb_generations' must be an integer"
  )
  expect_error(
    run_simulation(sampling_frequency = 12.3, output_path = NULL),
    "'sampling_frequency' must be an integer"
  )
  expect_error(
    run_simulation(seed = 1.4, output_path = NULL),
    "'seed' must be an integer"
  )
  expect_error(
    run_simulation(nb_generations = 1.4, output_path = NULL),
    "'nb_generations' must be an integer"
  )
  expect_error(
    run_simulation(nb_generations = 0, output_path = NULL),
    "'nb_generations' contains forbidden values: 0"
  )
  expect_error(
    run_simulation(nb_generations = Inf, output_path = NULL),
    "'nb_generations' contains forbidden values: Inf"
  )
  expect_error(
    run_simulation(prob_mutation = 15, output_path = NULL),
    "'prob_mutation' must be a numeric between 0 and 1"
  )

})

test_that("unix_tests", {
  if (Sys.getenv("TRAVIS") != "") {
    expect_error(
      run_simulation(output_path = NULL, hpc_job_id = 0.999),
      "'hpc_job_id' must be an integer"
    )
  } else {
    testthat::skip("Run only on Unix")
  }

})
