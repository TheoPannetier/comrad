#' Run the simulation
#'
#' Run the competitive radiation simulation.
#'
#' @param path_to_output character, path to save the output file, which must be a
#' `.csv`. If `NULL`, the output is not saved and the final state of the
#' community is returned at the end of the simulation.
#' @param init_comm The initial community, must have the same tibble structure
#' as the [default_init_comm()], which contains 10 individuals with `z = 0`
#' @param nb_gens integer, the number of generations to run the
#' simulation for.
#' @param sampling_freq numeric \code{> 0}, the frequency (in generations) at
#' which the community is written to output. See [set_sampling_freq()] for the
#' default option.
#' @param sampling_frac numeric (between 0 and 1), fraction of the community
#' (in terms of individuals) written to output at every sampled generation. A
#' truncation is operated.
#' @param seed integer \code{> 0}, the seed to set for the random number
#' generator. Defaults to an integer based on current day and time.
#' @inheritParams default_params_doc
#' @param hpc_job_id used to record a job ID in the metadata, only relevant for
#' simulations run on a high-performance cluster. Otherwise takes value
#' `"local"`.
#'
#' @return Returns a table with a row corresponding to each individual, and five
#' columns: `t` is the generation time, `z` the individual's trait value,
#' `species` the name of the species it belongs to, and `ancestral_species` the
#' previous species it descends from.
#' If `path_to_output = NULL`, the community at the last generation is returned.
#' If the path to a `.csv` file is supplied, each sampled generation is appended
#' to the file. In the `.csv`, the table is preceded by 17 lines of metadata,
#' which are automatically ignored if the file is read with [read_comrad_tbl()].
#'
#' @author Théo Pannetier
#' @export
#'
run_simulation <- function(
  path_to_output,
  nb_gens,
  init_comm = default_init_comm(),
  growth_rate = default_growth_rate(),
  competition_sd = default_competition_sd(),
  carrying_cap_sd = default_carrying_cap_sd(),
  carrying_cap_opt = default_carrying_cap_opt(),
  trait_opt = default_trait_opt(),
  prob_mutation = default_prob_mutation(),
  mutation_sd = default_mutation_sd(),
  trait_dist_sp = default_trait_dist_sp(),
  sampling_freq = comrad::set_sampling_freq(nb_gens),
  sampling_frac = default_sampling_frac(),
  seed = default_seed(),
  hpc_job_id = NULL
) {
  comrad::test_comrad_comm(init_comm)
  if (!is.null(path_to_output)) {
    if (!is.character(path_to_output)) {
      stop("'path_to_output' must be either null or a character.")
    } else {
      path_to_output_extension <- substr(
        path_to_output,
        nchar(path_to_output) - 3,
        nchar(path_to_output)
      )
      if (!path_to_output_extension == ".csv") {
        stop("'path_to_output' must be a .csv")
      }
    }
  }

  comrad::testarg_num(nb_gens)
  comrad::testarg_pos(nb_gens)
  comrad::testarg_not_this(nb_gens, c(0, Inf))
  comrad::testarg_int(nb_gens)
  comrad::testarg_num(sampling_freq)
  comrad::testarg_int(sampling_freq)
  comrad::testarg_num(seed)
  comrad::testarg_int(seed)
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(competition_sd)
  comrad::testarg_pos(competition_sd)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carrying_cap_opt)
  comrad::testarg_pos(carrying_cap_opt)
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd)
  comrad::testarg_num(prob_mutation)
  comrad::testarg_prop(prob_mutation)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)
  comrad::testarg_num(trait_dist_sp)
  comrad::testarg_pos(trait_dist_sp)
  comrad::testarg_num(sampling_frac)
  comrad::testarg_prop(sampling_frac)

  is_on_unix <- rappdirs::app_dir()$os == "unix" # for the cluster

  if (is_on_unix) {
    if (!is.null(hpc_job_id)) {
      comrad::testarg_num(hpc_job_id)
      comrad::testarg_int(hpc_job_id)
    }
  } else {
    hpc_job_id <- "local" # brute force
  }

  # Send metadata to output
  metadata_string <- paste(
    "### Metadata ###",
    "\ncompetition_sd =", competition_sd,
    "\ncarrying_cap_sd =", carrying_cap_sd,
    "\ncarrying_cap_opt =", carrying_cap_opt,
    "\ntrait_opt =", trait_opt,
    "\ngrowth_rate =", growth_rate,
    "\nprob_mutation =", prob_mutation,
    "\nmutation_sd =", mutation_sd,
    "\ntrait_dist_sp =", trait_dist_sp,
    "\n",
    "\nseed =", seed,
    "\nHPC job ID =", hpc_job_id,
    "\nsimulated under comrad", as.character(utils::packageVersion("comrad")),
    "\n", R.version$version.string,
    "\n",
    "\nRunning for", nb_gens, "generations",
    "\n"
  )
  if (is_on_unix) {
    cat(metadata_string)
  }

  if (!is.null(path_to_output)) {
    cat(
      metadata_string,
      # Set up output table
      "\n### Simulation output ###",
      "\n",
      "\nt,z,species,ancestral_species\n",
      file = path_to_output
    )
  }

  # Set up data output table proper
  output <- tibble::tibble(
    "t" = 0,
    "z" = init_comm$z,
    "species" = init_comm$species,
    "ancestral_species" = as.character(NA)
  )
  if (!is.null(path_to_output)) {
    readr::write_csv(
      output,
      path = path_to_output,
      append = TRUE
    )
  }

  # Set initial community
  comm <- init_comm
  # Set timer
  start_time <- proc.time()[3]
  # Let's not forget the seed
  set.seed(seed)

  # Go :)
  for (t in 1:nb_gens) {

    # Replace comm with next generation
    comm <- comrad::draw_comm_next_gen(
      comm = comm,
      growth_rate = growth_rate,
      competition_sd = competition_sd,
      trait_opt = trait_opt,
      carrying_cap_opt = carrying_cap_opt,
      carrying_cap_sd = carrying_cap_sd,
      prob_mutation = prob_mutation,
      mutation_sd = mutation_sd,
      trait_dist_sp = trait_dist_sp,
      seed = seed
    )

    if (length(comm$species) < 1) {
      cat("\nCommunity has gone extinct at generation", t, "\n")
      if (is.null(path_to_output)) {
        return(output)
      } else {
        return()
      }
    }

    output <- tibble::tibble(
      "t" = t,
      "z" = comm$z,
      "species" = comm$species,
      "ancestral_species" = comm$ancestral_species
    )

    if (t %% sampling_freq == 0) {
      cat("\nRunning generation", t, "/", nb_gens)
      if (!is.null(path_to_output)) {
        # Write only a sample of the output
        sampled_output <- comrad::sample_output(
          output = output,
          sampling_frac = sampling_frac
        )
        readr::write_csv(
          sampled_output,
          path = path_to_output,
          append = TRUE
        )
      }
    }
  }

  cat("\nTotal runtime:", proc.time()[3] - start_time, "\n")

  if (is.null(path_to_output)) {
    return(output)
  } else {
    return()
  }
}
