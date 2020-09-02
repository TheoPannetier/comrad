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

  spp <- unique(comrad_tbl$species)
  names(spp) <- spp # for automatied labelling by map_dfr

  phylo_tbl <- purrr::map_dfr(
    spp,
    function (sp) {
      is_sp <- comrad_tbl$species == sp

      time_range <- comrad_tbl$t[is_sp]

      ancestor_name <- unique(
        comrad_tbl$ancestral_species[is_sp]
      )

      sp_entry <- tibble::tibble(
        "ancestor_name" = ancestor_name,
        "time_birth" = min(time_range),
        "time_death" = max(time_range)
      )
    },
    .id = "species_name"
  )
  return(phylo_tbl)
}
