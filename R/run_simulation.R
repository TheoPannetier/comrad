#' Run the simulation
#'
#' Run the competitive radiation simulation.
#'
#' @param init_pop numeric vector, the trait values of the initial individuals.
#' @param output_path character, path to save the output file.
#' @param sampling_frequency numeric \code{> 0}, the frequency at which the
#' population is saved in the output.
#' @param seed numeric \code{> 0}, the integer seed to set for the random number
#' generator.
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
  init_pop,
  output_path,
  nb_generations,
  sampling_frequency = set_sampling_frequency(nb_generations),
  seed = default_seed(),
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  mutation_sd = default_mutation_sd()
) {
  testarg_num(init_pop)
  testarg_char(output_path)
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
  testarg_num(mutation_sd)
  testarg_pos(mutation_sd)

  # other arguments are tested in run_generation_step()

  # Send metadata to output
  cat(
    "### Metadata ###",
    "\ngrowth_rate =", growth_rate,
    "\ncomp_width =", comp_width,
    "\ntrait_opt =", trait_opt,
    "\ncarr_cap_opt =", carr_cap_opt,
    "\ncarr_cap_width =", carr_cap_width,
    "\nmutation_sd =", mutation_sd,
    "\n",
    "\nseed =", seed,
    "\n",
    "\nRunning for", nb_generations, "generations",
    "\n",
    # Set up output table
    "\n### Simulation output ###",
    "\n",
    "\nt,z,runtime\n",
    file = output_path
  )
  # Set up data output table proper
  readr::write_csv(
    as.data.frame(cbind(
      0, # generation
      init_pop, # initial pop trait values
      0 # starting time
    )),
    path = output_path,
    append = TRUE
  )

  # Set initial population
  parent_pop <- init_pop

  # Set timer
  start_time <- proc.time()[3]
  gen_time <- start_time # updated within the loop

  # Go :)
  for (t in 1:nb_generations) {

    cat("\nRunning generation", t, "/", nb_generations)

    offspring_pop <- run_generation_step(
      traits_pop = parent_pop,
      growth_rate = growth_rate,
      comp_width = comp_width,
      trait_opt = trait_opt,
      carr_cap_opt = carr_cap_opt,
      carr_cap_width = carr_cap_width,
      mutation_sd = mutation_sd
    )
    if (offspring_pop[1] == "Extinct") { # calling [1] silences warning
      cat(
        "\n",
        "\nPopulation has gone extinct at generation", t,
        file = output_path,
        append = TRUE
      )
      cat("\nPopulation has gone extinct at generation", t, "\n")
      return()
    }

    parent_pop <- offspring_pop

    if (t %% sampling_frequency == 0) {
      readr::write_csv(
        as.data.frame(cbind(
          t,
          parent_pop,
          proc.time()[3] - gen_time # generation runtime
        )),
        path = output_path,
        append = TRUE
      )
    }

    gen_time <- proc.time()[3]
  }

  cat(
    "\n", "\n Total runtime:", proc.time()[3] - start_time,
    file = output_path,
    append = TRUE
  )
}
