#' Run the simulation
#'
#' Run the competitive radiation simulation.
#'
#' @param init_pop a tibble containing the initial population.
#' @param output_path character, path to save the output file. If `NULL`, the
#' output is not saved and the population is returned at the end of the
#' simulation.
#' @param sampling_frequency numeric \code{> 0}, the frequency at which the
#' population is saved in the output.
#' @param seed numeric \code{> 0}, the integer seed to set for the random number
#' generator.
#' @param plot_every if a numeric is supplied the simulation
#' will plot its current state every `plot_every`. If `null` no plot is
#' produced. Silently set to `NULL` if not run from Rstudio.
#' @inheritParams default_params_doc
#'
#' @details Output is registered in a .csv file with the following structure:
#' | t | z | runtime |
#' | --- | --- |--- |
#' | --- | --- |--- |
#' where each line is an individual (the entire population is recorded).
#' \code{t} denotes the generation counter, \code{z} is a trait value, and
#' \code{runtime} is the processing time elapsed during the current generation.
#' In addition, the output table is preceded by metadata. Skip the 14 first
#' lines to get the table proper.
#'
#' @author Theo Pannetier
#' @export
#'
run_simulation <- function(
  init_pop = default_init_pop(),
  output_path = NULL,
  nb_generations = 20,
  sampling_frequency = set_sampling_frequency(nb_generations),
  seed = default_seed(),
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  prob_mutation = default_prob_mutation(),
  mutation_sd = default_mutation_sd(),
  fitness_func = fitness_func_ricker,
  plot_every = NULL
) {
  test_comrad_pop(init_pop)
  if (!is.null(output_path) && !is.character(output_path)) {
    stop("'output_path' must be either null or a character.")
  }
  testarg_num(sampling_frequency)
  testarg_int(sampling_frequency)
  testarg_num(seed)
  testarg_int(seed)
  testarg_num(nb_generations)
  testarg_int(nb_generations)
  testarg_not_this(nb_generations, c(0, Inf))
  testarg_num(growth_rate)
  testarg_pos(growth_rate)
  testarg_num(comp_width)
  testarg_pos(comp_width)
  testarg_num(trait_opt)
  testarg_num(carr_cap_opt)
  testarg_pos(carr_cap_opt)
  testarg_num(carr_cap_width)
  testarg_pos(carr_cap_width)
  testarg_num(prob_mutation)
  testarg_prop(prob_mutation)
  testarg_num(mutation_sd)
  testarg_pos(mutation_sd)
  if (Sys.getenv("RSTUDIO") != "1") {
    plot_every <- NULL
  }
  if (!is.null(plot_every) && !is.numeric(plot_every)) {
    stop("plot_every must be null or numeric.")
  }
  # Send metadata to output
  if (!is.null(output_path)) {
    cat(
      "### Metadata ###",
      "\ngrowth_rate =", growth_rate,
      "\ncomp_width =", comp_width,
      "\ntrait_opt =", trait_opt,
      "\ncarr_cap_opt =", carr_cap_opt,
      "\ncarr_cap_width =", carr_cap_width,
      "\nprob_mutation =", prob_mutation,
      "\nmutation_sd =", mutation_sd,
      "\n",
      "\nseed =", seed,
      "\nsimulated under comrad", as.character(utils::packageVersion("comrad")),
      "\n",
      "\nRunning for", nb_generations, "generations",
      "\n",
      # Set up output table
      "\n### Simulation output ###",
      "\n",
      "\nt,z,species,ancestral_species,runtime\n",
      file = output_path
    )
  }
  # Set up data output table proper
  fossil_record <- tibble::tibble(
    "t" = 0,
    "z" = init_pop$z,
    "species" = init_pop$species,
    "ancestral_species" = as.character(NA),
    "runtime" = 0
  )

  if (!is.null(output_path)) {
    readr::write_csv(
      fossil_record,
      path = output_path,
      append = TRUE
    )
  }

  # Set initial population
  pop <- init_pop

  # Set timer
  start_time <- proc.time()[3]

  # Go :)
  for (t in 1:nb_generations) {

    cat("Running generation", t, "/", nb_generations, "\n")
    gen_time <- proc.time()[3]

    # Replace pop with next generation
    pop <- draw_next_gen(
      pop = pop,
      growth_rate = growth_rate,
      comp_width = comp_width,
      trait_opt = trait_opt,
      carr_cap_opt = carr_cap_opt,
      carr_cap_width = carr_cap_width,
      prob_mutation = prob_mutation,
      mutation_sd = mutation_sd,
      fitness_func = fitness_func
    )

    if (length(pop$species) < 1) {
      if (!is.null(output_path)) {
        cat(
          "\nPopulation has gone extinct at generation", t, "\n",
          file = output_path,
          append = TRUE
        )
        cat("\nPopulation has gone extinct at generation", t, "\n")
        return(0) # still a normal conclusion of the simulation
      } else {
        cat("\nPopulation has gone extinct at generation", t, "\n")
        return(fossil_record)
      }
    }

    fossil_entry <- tibble::tibble(
      "t" = t,
      "z" = pop$z,
      "species" = pop$species,
      "ancestral_species" = pop$ancestral_species,
      "runtime" = proc.time()[3] - gen_time
    )

    if (!is.null(output_path) && t %% sampling_frequency == 0) {
      readr::write_csv(
        fossil_entry,
        path = output_path,
        append = TRUE
      )
    }

    fossil_record <- rbind(fossil_record, fossil_entry)

    if (!is.null(plot_every) && (t %% plot_every == 0)) {
      # plot_population_trait_evolution(output)
    }
  }

  cat(
    "\n", "\n Total runtime:", proc.time()[3] - start_time,
    file = ifelse(is.null(output_path), "", output_path),
    append = TRUE
  )
  if (is.null(output_path)) {
    return(fossil_record)
  } else {
    return(0)
  }
}
