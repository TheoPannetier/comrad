context("test-create_next_gen_traits")

test_traits <- c(0.5, 1.2, -0.8)
test_nb_offspring <- c(2, 3, 1)

test_that("use", {
  # Ordinary cases
  expect_length(create_next_gen_traits(rnorm(3), 1:3), 6)
  # Border cases
  expect_equal(
    create_next_gen_traits(rnorm(3), rep(0, 3)),
    "Extinct"
    )
})

test_that("abuse", {
  expect_error(
    create_next_gen_traits(rnorm(3), Inf),
    "'nb_offspring_pop' contains forbidden values: Inf"
  )
  expect_error(
    create_next_gen_traits("superior programming", 3),
    "'traits_pop' must be numeric"
  )
  expect_error(
    create_next_gen_traits(rnorm(3), 3),
    "'nb_offspring_pop' must have length 3"
  )
})
