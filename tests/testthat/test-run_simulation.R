context("test-run_simulation")

test_that("diverging_population", {
  # Population starts with split values, expect speciation in the next step
  diverging_pop <- default_init_pop()
  diverging_pop$z[6:10] <- 0.1
  output <- run_simulation(init_pop = diverging_pop, nb_generations = 1)
  species <- output %>%
    dplyr::filter(t == 1) %>%
    dplyr::select(species) %>%
    unique %>% unlist()
  ancestral_species <- output %>%
    dplyr::filter(t == 1) %>%
    dplyr::select(ancestral_species) %>%
    unique() %>% unlist()
  expect_gt(length(species), 1)
  expect_gt(length(ancestral_species), 1)
})

test_that("output_format", {
  nb_generations <- 5
  gen_seq <- seq(0, nb_generations, by = set_sampling_frequency(nb_generations))
  output <- run_simulation(nb_generations = nb_generations)

  expect_equal(length(output), 5)
  # Main population columns
  expect_silent(
    output[, c("z", "species", "ancestral_species")] %>% test_comrad_pop()
  )
  # Format of columns missed by test_comrad_pop()
  expect_true(any(output$t >= 0))
  expect_true(any(output$runtime >= 0))
  # Assert all expected generations have been sampled
  expect_equal(unique(output$t), gen_seq)
})

test_that("extinction", {
  expect_output(
    # TPK
    output <- run_simulation(carr_cap_opt = 0, nb_generations = 1),
    "\\nRunning generation 1 / 1\\nPopulation has gone extinct at generation 1 "
  )
})

test_that("parameter_abuse", {
  expect_error(
    run_simulation(init_pop = rep(0, 10)),
    "'init_pop' should be a tibble."
  )
  expect_error(
    run_simulation(output_path = 1),
    "'output_path' must be either null or a character."
  )
  expect_error(
    run_simulation(nb_generations = 12.3),
    "'nb_generations' must be an integer"
  )
  expect_error(
    run_simulation(sampling_frequency = 12.3),
    "'sampling_frequency' must be an integer"
  )
  expect_error(
    run_simulation(seed = 1.4),
    "'seed' must be an integer"
  )
  expect_error(
    run_simulation(nb_generations = 1.4),
    "'nb_generations' must be an integer"
  )
  expect_error(
    run_simulation(nb_generations = 0),
    "'nb_generations' contains forbidden values: 0"
  )
  expect_error(
    run_simulation(nb_generations = Inf),
    "'nb_generations' contains forbidden values: Inf"
  )
  expect_error(
    run_simulation(prob_mutation = 15),
    "'prob_mutation' must be a numeric between 0 and 1"
  )


})
