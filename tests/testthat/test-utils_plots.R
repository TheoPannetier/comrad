context("test-utils")

# There are 12 DD models combinations
expect_equal(length(dd_models()), 12)
dd_models() %>% purrr::walk(function(dd_model) {
  dd_name <- dd_model$name
  # The model is accepted
  expect_silent(dd_model_comrad_to_ddd(dd_name))
})

# Not all combinations are supported
expect_error(
  dd_model_comrad_to_ddd("cp"),
  "This DD model is not supported in comrad."
)

context("test-plots")
# Plots execute without error
aicw_tbl <- tibble::tibble(
  "dd_model" = c("xx", "lc", "xl"),
  "aicw" = c(0.7, 0.2, 0.1)
)
expect_silent(plot_aicw_pie(aicw_tbl))
expect_silent(plot_dd_func_colours())
expect_silent(plot_dd_model_colours())
params <- c("lambda_0" = 2, "mu_0" = 0.5, "k" = 30, "alpha" = 0.5)
expect_silent(plot_compare_dd_functions(params = params, N = 0:35))
