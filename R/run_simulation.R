#' Run the simulation
#'
#' Run the competitive radiation simulation.
#'
#' @param output_path character, path to save the output file, which must be a
#' `.csv`. If `NULL`, the output is not saved and the final state of the
#' community is returned at the end of the simulation.
#' @param init_comm The initial community, must have the same tibble structure
#' as the [default_init_comm()], which contains 10 individuals with `z = 0`
#' @param nb_generations integer, the number of generations to run the
#' simulation for.
#' @param sampling_frequency numeric \code{> 0}, the frequency at which the
#' community is saved in the output. See [set_sampling_frequency()] for the
#' default option.
#' @param sampling_prop numeric (between 0 and 1), the proportion of
#' individuals. The fraction of individuals returned is approximative
#' because of a truncation.
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
#' If `output_path = NULL`, the community at the last generation is returned.
#' If the path to a `.csv` file is supplied, each sampled generation is appended
#' to the file. In the `.csv`, the table is preceded by 17 lines of metadata,
#' which are automatically ignored if the file is read with [read_comrad_tbl()].
#'
#' @author Théo Pannetier
#' @export
#'
run_simulation <- function(
  output_path,
  init_comm = default_init_comm(),
  nb_generations = 20,
  sampling_frequency = comrad::set_sampling_frequency(nb_generations),
  seed = default_seed(),
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  prob_mutation = default_prob_mutation(),
  mutation_sd = default_mutation_sd(),
  trait_gap = default_trait_gap(),
  sampling_prop = default_sampling_prop(),
  hpc_job_id = NULL
) {
  comrad::test_comrad_comm(init_comm)
  if (!is.null(output_path)) {
    if (!is.character(output_path)) {
      stop("'output_path' must be either null or a character.")
    } else {
      output_path_extension <- substr(
        output_path,
        nchar(output_path) - 3,
        nchar(output_path)
      )
      if (!output_path_extension == ".csv") {
        stop("'output_path' must be a .csv")
      }
    }
  }

  comrad::testarg_num(nb_generations)
  comrad::testarg_pos(nb_generations)
  comrad::testarg_not_this(nb_generations, c(0, Inf))
  comrad::testarg_int(nb_generations)
  comrad::testarg_num(sampling_frequency)
  comrad::testarg_int(sampling_frequency)
  comrad::testarg_num(seed)
  comrad::testarg_int(seed)
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(comp_width)
  comrad::testarg_pos(comp_width)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carr_cap_opt)
  comrad::testarg_pos(carr_cap_opt)
  comrad::testarg_num(carr_cap_width)
  comrad::testarg_pos(carr_cap_width)
  comrad::testarg_num(prob_mutation)
  comrad::testarg_prop(prob_mutation)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)
  comrad::testarg_num(trait_gap)
  comrad::testarg_pos(trait_gap)
  comrad::testarg_num(sampling_prop)
  comrad::testarg_prop(sampling_prop)

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
    "\ngrowth_rate =", growth_rate,
    "\ncomp_width =", comp_width,
    "\ntrait_opt =", trait_opt,
    "\ncarr_cap_opt =", carr_cap_opt,
    "\ncarr_cap_width =", carr_cap_width,
    "\nprob_mutation =", prob_mutation,
    "\nmutation_sd =", mutation_sd,
    "\n",
    "\nseed =", seed,
    "\nHPC job ID =", hpc_job_id,
    "\nsimulated under comrad", as.character(utils::packageVersion("comrad")),
    "\n", R.version$version.string,
    "\n",
    "\nRunning for", nb_generations, "generations",
    "\n"
  )
  if (is_on_unix) {
    cat(metadata_string)
  }

  if (!is.null(output_path)) {
    cat(
      metadata_string,
      # Set up output table
      "\n### Simulation output ###",
      "\n",
      "\nt,z,species,ancestral_species\n",
      file = output_path
    )
  }

  # Set up data output table proper
  output <- tibble::tibble(
    "t" = 0,
    "z" = init_comm$z,
    "species" = init_comm$species,
    "ancestral_species" = as.character(NA)
  )
  if (!is.null(output_path)) {
    readr::write_csv(
      output,
      path = output_path,
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
  for (t in 1:nb_generations) {

    # Replace comm with next generation
    comm <- comrad::draw_comm_next_gen(
      comm = comm,
      growth_rate = growth_rate,
      comp_width = comp_width,
      trait_opt = trait_opt,
      carr_cap_opt = carr_cap_opt,
      carr_cap_width = carr_cap_width,
      prob_mutation = prob_mutation,
      mutation_sd = mutation_sd,
      trait_gap = trait_gap,
      seed = seed
    )

    if (length(comm$species) < 1) {
      cat("\nCommunity has gone extinct at generation", t, "\n")
      if (is.null(output_path)) {
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

    if (t %% sampling_frequency == 0) {
      cat("\nRunning generation", t, "/", nb_generations)
      if (!is.null(output_path)) {
        # Write only a sample of the output
        sampled_output <- comrad::sample_output(
          output = output,
          sampling_prop = sampling_prop
        )
        readr::write_csv(
          sampled_output,
          path = output_path,
          append = TRUE
        )
      }
    }
  }

  cat("\nTotal runtime:", proc.time()[3] - start_time, "\n")

  if (is.null(output_path)) {
    return(output)
  } else {
    return()
  }
}
