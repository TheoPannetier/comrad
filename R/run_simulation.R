#' Run the simulation
#'
#' Run the individual-based model simulation.
#'
#' @param path_to_output character, path to save the output file, which must be
#' a `.csv`. If `NULL`, the output is not saved and the final state of the
#' community is returned at the end of the simulation.
#' @param init_comm The initial community. Default is [default_init_comm()], any
#' other input should have the same structure, i.e. a data frame with four
#' columns, "t" / "z" / "species" / "ancestral_species", and one row for each
#' individual in the community.
#' @param init_comm_is_from optional string passed to metadata, where the
#' initial community was taken from (a filename or anything else).
#' @param nb_gens integer, the number of generations to run the
#' simulation for.
#' @param sampling_on_event logical. If `TRUE`, the community is sampled every
#' time a speciation or extinction happens, and `sampling_freq` is ignored and
#' must be set to `NA`.
#' @param sampling_freq numeric \code{> 0}, the frequency (in generations) at
#' which the community is written to output. See [set_sampling_freq()] for the
#' default option.
#' @param sampling_frac numeric (between 0 and 1), fraction of the community
#' (in terms of individuals) written to output at every sampled generation. A
#' truncation is operated.
#' @param seed integer \code{> 0}, the seed to set for the random number
#' generator. Defaults to an integer based on current day and time.
#' @inheritParams default_params_doc
#' @param switch_carr_cap_sd_after if not `NA`, the value of `carrying_cap_sd` will
#' switch to `switch_carr_cap_sd_to` at t = `switch_carr_cap_sd_after` + 1.
#' @param switch_carr_cap_sd_to the value to switch `carrying_cap_sd` to if
#' `switch_carr_cap_sd_after` is not `NA`.
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
#' to the file. In the `.csv`, the table is preceded by some lines of metadata,
#' which are automatically ignored if the file is read with [read_comrad_tbl()].
#'
#' @author Théo Pannetier
#' @export
#'
run_simulation <- function( # nolint, ignore high cyclomatic complexity
  path_to_output,
  nb_gens,
  init_comm = comrad::default_init_comm(),
  init_comm_is_from = NA,
  growth_rate = comrad::default_growth_rate(),
  competition_sd = comrad::default_competition_sd(),
  carrying_cap_sd = comrad::default_carrying_cap_sd(),
  carrying_cap_opt = comrad::default_carrying_cap_opt(),
  trait_opt = comrad::default_trait_opt(),
  mutation_sd = comrad::default_mutation_sd(),
  trait_dist_sp = comrad::default_trait_dist_sp(),
  switch_carr_cap_sd_after = NA,
  switch_carr_cap_sd_to = NA,
  sampling_on_event = FALSE,
  sampling_freq = ifelse(
    sampling_on_event, NA, comrad::set_sampling_freq(nb_gens)
  ),
  sampling_frac = comrad::default_sampling_frac(),
  seed = comrad::default_seed(),
  hpc_job_id = NULL
) {
  comrad::test_comrad_tbl(init_comm)
  first_gen <- init_comm %>% dplyr::pull(t) %>% unique()
  if (length(first_gen) > 1) {
    stop("\"init_comm\" should contain a single generation.")
  }
  if (!is.na(init_comm_is_from)) {
    testarg_char(init_comm_is_from)
  }
  if (!is.null(path_to_output)) {
    if (!is.character(path_to_output)) {
      stop("'path_to_output' must be either null or a character.")
    } else {
      if (!path_to_output %>% stringr::str_detect("\\.csv$")) {
        stop("'path_to_output' must be a .csv")
      }
    }
  }
  comrad::testarg_num(nb_gens)
  comrad::testarg_pos(nb_gens)
  comrad::testarg_not_this(nb_gens, c(0, Inf))
  comrad::testarg_int(nb_gens)
  comrad::testarg_log(sampling_on_event)
  if (sampling_on_event) {
    if (!is.na(sampling_freq)) {
      stop("If \"sampling_on_event\" is TRUE \"sampling_freq\" must be NA.")
    }
  } else {
    comrad::testarg_num(sampling_freq)
    comrad::testarg_int(sampling_freq)
  }
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
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)
  comrad::testarg_num(trait_dist_sp)
  comrad::testarg_pos(trait_dist_sp)
  comrad::testarg_num(sampling_frac)
  comrad::testarg_prop(sampling_frac)

  will_switch <- !is.na(switch_carr_cap_sd_after)
  if (will_switch) {
    comrad::testarg_num(switch_carr_cap_sd_after)
    comrad::testarg_num(switch_carr_cap_sd_to)
    comrad::testarg_pos(switch_carr_cap_sd_to)
  }

  is_on_peregrine <- Sys.getenv("HOSTNAME") == "peregrine.hpc.rug.nl"

  if (is_on_peregrine) {
    if (!is.null(hpc_job_id)) {
      comrad::testarg_num(hpc_job_id)
      comrad::testarg_int(hpc_job_id)
    }
  } else {
    hpc_job_id <- "local"
  }

  # Prepare metadata
  metadata_string <- paste0(
    "### Metadata ###",
    "\ncompetition_sd = ", competition_sd,
    "\ncarrying_cap_sd = ", carrying_cap_sd,
    "\ncarrying_cap_opt = ", carrying_cap_opt,
    "\ntrait_opt = ", trait_opt,
    "\ngrowth_rate = ", growth_rate,
    "\nmutation_sd = ", mutation_sd,
    "\ntrait_dist_sp = ", trait_dist_sp,
    "\n",
    "\nseed = ", seed,
    "\nHPC job ID = ", hpc_job_id,
    "\nsimulated under comrad ", as.character(utils::packageVersion("comrad")),
    "\n", R.version$version.string,
    "\n",
    "\nStarting community from: ", init_comm_is_from,
    "\nRunning for ", nb_gens, " generations",
    "\n"
  )
  if (is_on_peregrine) {
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
  comrad_tbl <- init_comm
  if (!is.null(path_to_output)) {
    readr::write_csv(
      comrad_tbl,
      file = path_to_output,
      append = TRUE
    )
  }
  # Let's not forget the seed
  set.seed(seed)

  # Go :)
  time_seq <- (first_gen + 1):(first_gen + nb_gens)
  for (t in time_seq) {
    if (will_switch && t > switch_carr_cap_sd_after) {
      cat("\nSwitched carrying_cap_sd from", carrying_cap_sd, "to",
          switch_carr_cap_sd_to, "at t =", t, "\n")
      carrying_cap_sd <- switch_carr_cap_sd_to
      will_switch <- FALSE
    }
    species_before <- unlist(dplyr::distinct(comrad_tbl, species))

    # Replace comm with next generation
    comrad_tbl <- dplyr::bind_cols(
      # Time [t] # nolint
      "t" = t,
      # Community next gen [z, species, ancestral_species]
      comrad::draw_comm_next_gen(
        comm = comrad_tbl[, -1],
        growth_rate = growth_rate,
        competition_sd = competition_sd,
        trait_opt = trait_opt,
        carrying_cap_opt = carrying_cap_opt,
        carrying_cap_sd = carrying_cap_sd,
        mutation_sd = mutation_sd,
        trait_dist_sp = trait_dist_sp
      )
    )
    species_after <- unlist(dplyr::distinct(comrad_tbl, species))

    if (nrow(comrad_tbl) < 1) {
      cat("\nCommunity has gone extinct at generation", t, "\n")
      if (is.null(path_to_output)) {
        return(comrad_tbl)
      } else {
        return()
      }
    }

    if (sampling_on_event) {
      # Sample if speciation or extinction
      sample_this_gen <- !setequal(species_before, species_after) || t == nb_gens
    } else {
      # Sample every sampling_freq generations
      sample_this_gen <- t %% sampling_freq == 0 || t == nb_gens
    }

    if (sample_this_gen) {
      if (!is.null(path_to_output)) {
        saved_seed <- .GlobalEnv$.Random.seed
        # Write only a sample of the output
        sampled_output <- comrad_tbl %>%
          dplyr::slice_sample(prop = sampling_frac)
        # Make sure all species are present in sample
        species_sampled <- unlist(dplyr::distinct(sampled_output, species))
        not_sampled <- setdiff(species_after, species_sampled)
        if (length(not_sampled) > 0) {
          for (sp in not_sampled) {
            sampled_output <- dplyr::bind_rows(
              sampled_output,
              comrad_tbl %>% dplyr::filter(species == sp)
            )
          }
        }
        readr::write_csv(
          sampled_output,
          file = path_to_output,
          append = TRUE
        )
        # Restore seed so sampling doesn't change course of simulation
        .GlobalEnv$.Random.seed <- saved_seed
      }
      cat("\nSampled generation", t, "/", time_seq[length(time_seq)])
    }
  }

  if (is.null(path_to_output)) {
    return(comrad_tbl)
  }
}
