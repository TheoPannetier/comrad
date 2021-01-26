#' Simulate a phylogeny with a diversity-dependent birth-death model
#'
#' Use a Gillespie algorithm to create a `phylo` object from the DD model
#' specified by `dd_model` and `params`. The phylogeny is conditioned on the
#' survival of at least one lineage to the end of the simulation.
#'
#' @param params a named vector containing the values of the parameters of the
#' DD model. Names and length must match those expected by `dd_model$params_check`
#' @param nb_gens integer, the number of generations the simulation should run
#' for.
#' @param dd_model a list with five named elements that together specify the
#' diversity-dependent model:
#' * `name` a two-letter code, the name of the model. First letter specifies the
#' form of the speciation function, second letter the form of the extinction
#' function: "l" for "linear", "x" for exponential, "c" for constant.
#' * `speciation_func`, a function specifying the diversity-dependent speciation
#' rate. Must take arguments `params` and `N`.
#' * `extinction_func`, a function specifying the diversity-dependent extinction
#' rate. Must take arguments `params` and `N`.
#' * `constraints` a list of conditions that parameter values must satisfy. Each
#' element is a function that takes arguments `params` and `...`, and returns
#' `TRUE` if the constraint is satisfied, `FALSE` if it isn't.
#' * `params_check` a function that controls the format of `params`. Returns an
#' error if the elements of `params` are named differently from what is expected
#' or if the length differs from the expectation.
#'
#' `comrad` contains several `dd_model` functions, see for example
#' [comrad::dd_model_lc()].
#'
#' @return a `phylo` object containign the simulated phylogeny, including
#' extinct lineages and the stem.
#'
#' @author Theo Pannetier
#' @export

simulate_dd_phylo <- function(params, nb_gens, dd_model) {

  kprime <- ceiling(params["k"] * params["lambda_0"] / (params["lambda_0"] - params["mu_0"]))
  n_max <- ceiling(kprime * 2)

  rate_tbl <- tibble::tibble(
    "N" = 1:n_max,
    "lambda" = dd_model$speciation_func(params = params, N = N),
    "mu" = dd_model$extinction_func(params = params, N = N),
    "total_rate" = (lambda + mu) * N,
    "p_speciation" = lambda / (lambda + mu)
  )
  if(any(!between(rate_tbl$p_speciation, 0, 1))) {
    stop("\"p_speciation\" is not a probability.")
  }

  has_survived <- FALSE
  while(!has_survived) {
    spp_tbl <- tibble::tibble(
      "species_name" = charlatan::ch_hex_color(),
      "ancestor_name" = as.character(NA),
      "time_birth" = 0,
      "time_death" = nb_gens
    )
    # Initialise variables
    alive <- spp_tbl$species_name[1]
    n_alive <- 1L
    rate <- rate_tbl$total_rate[1]
    gen <- ceiling(stats::rexp(1, rate = rate))

    while(gen <= nb_gens) {
      # Resolve event
      target <- sample(alive, 1)
      p_spec <- rate_tbl$p_speciation[rate_tbl$N == n_alive]
      is_speciation <- p_spec >= stats::runif(1)
      if (is_speciation) {
        # Speciation
        spp_tbl <- dplyr::bind_rows(
          spp_tbl,
          # New sp
          tibble::tibble(
            "species_name" = charlatan::ch_hex_color(),
            "ancestor_name" = target,
            "time_birth" = gen,
            "time_death" = nb_gens
          )
        )
      } else {
        # Extinction
        spp_tbl$time_death[spp_tbl$species_name == target] <- gen
      }
      # Update variables
      alive <- spp_tbl$species_name[spp_tbl$time_death == nb_gens]
      n_alive <- length(alive)
      if (n_alive == 0) {
        break()
      } else if (n_alive > n_max) {
        stop("\"n_alive\" has become larger than \"n_max\"")
      }
      rate <- rate_tbl$total_rate[rate_tbl$N == n_alive]
      if (rate == 0) {
        break()
      }
      gen <- gen + ceiling(stats::rexp(1, rate = rate))
    }
    has_survived <- n_alive > 0
  }

  phylo <- spp_tbl %>%
    comrad::write_newick_str(include_stem = TRUE) %>%
    ape::read.tree(text = .)
  return(phylo)
}
