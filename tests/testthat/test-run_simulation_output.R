context("test-run_simulation_output")

test_that("standard_output_file", {
  temp_output_path <- paste0(tempfile("comrad_test_output"), ".csv")
  run_simulation(output_path = temp_output_path, nb_generations = 5)

  expect_silent(comrad_tbl <- read_comrad_tbl(temp_output_path))

  unlink(temp_output_path)
})
