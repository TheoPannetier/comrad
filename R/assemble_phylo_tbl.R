#' Extracts phylogenetic information from a `comrad` simulation
#'
#' Reads the output of a `comrad` simulation and creates a table containing
#' the necessary information about every species to make a phylogeny.
#'
#' @inheritParams default_params_doc
#'
#' @return a tibble with four columns and a row per species in the simulation
#' * `species_name`, name of the species
#' * `ancestor_name`, name of the ancestral species from which it branched. `NA`
#' for the initial species.
#' * `time_birth`, the generation at which the species appeared.
#' * `time_death`, the generation at which the last individual died. Last
#' generation if the species was still extant at the end of the simulation.
#'
#' @author Théo Pannetier
#' @export

assemble_phylo_tbl <- function(comrad_tbl) {
  # Stupid but necessary for the build
  species <- NULL
  ancestral_species <- NULL

  phylo_tbl <- tibble::tibble(
    "species_name" = character(),
    "ancestor_name" = character(),
    "time_birth" = numeric(),
    "time_death" = numeric()
  )

  for (sp in unique(comrad_tbl$species)) {

    time_birth <- comrad_tbl %>%
      dplyr::filter(species == sp) %>%
      dplyr::select(t) %>%
      unlist() %>%
      min()
    time_death <- comrad_tbl %>%
      dplyr::filter(species == sp) %>%
      dplyr::select(t) %>%
      unlist() %>%
      max()
    ancestor_name <- comrad_tbl %>%
      dplyr::filter(species == sp) %>%
      dplyr::select(ancestral_species) %>%
      unlist() %>%
      unique()

    sp_entry <- tibble::tibble(
      "species_name" = sp,
      "ancestor_name" = ancestor_name,
      "time_birth" = time_birth,
      "time_death" = time_death
    )

    phylo_tbl <- rbind(phylo_tbl, sp_entry)
  }
  phylo_tbl

}
